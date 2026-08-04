# frozen_string_literal: true

require "test_helper"
require "rubydex/server"
require "socket"
require "tmpdir"

module Rubydex
  module Server
    # A resident server serves every client, so one bad request, unreadable file, or bug must not
    # stop it for the rest.
    class CoreTest < Minitest::Test
      def setup
        skip("server mode is not supported on this platform") unless Server.supported?

        @runtime_dir = Dir.mktmpdir("rdx-core-test")
        @workspace = Dir.mktmpdir("rdx-core-workspace")
        @previous_server_dir = ENV["RDX_SERVER_DIR"]
        ENV["RDX_SERVER_DIR"] = @runtime_dir
        @state = State.new(workspace_path: @workspace)
        @state.record!
      end

      def teardown
        ENV["RDX_SERVER_DIR"] = @previous_server_dir
        FileUtils.rm_rf(@runtime_dir)
        FileUtils.rm_rf(@workspace)
      end

      # An error in one command must not reach `run` and stop the server for every other client;
      # it must cost one connection.
      def test_an_unexpected_error_answers_the_client_and_keeps_the_server
        core = Core.new(@state)
        core.define_singleton_method(:dispatch) { |_request, _client| raise "boom" }

        response = exchange_with(core, { "command" => "query", "query" => "MATCH (n) RETURN n" })

        assert_equal(1, response["status"])
        assert_match(/internal error/, response["stderr"])
        assert_match(/boom/, response["stderr"])

        assert_match(/internal error: RuntimeError: boom/, @log)
        assert_match(/core_test\.rb/, @log, "the log must keep the backtrace")
      end

      def test_an_unauthorized_request_is_refused_and_never_dispatched
        core = Core.new(@state)
        dispatched = false
        core.define_singleton_method(:dispatch) { |_request, _client| dispatched = true }

        response = exchange_with(core, { "command" => "query" }, token: "not-the-token")

        assert_equal(1, response["status"])
        assert_match(/unauthorized/, response["stderr"])
        refute(dispatched, "an unauthorized request must never reach dispatch")
      end

      # The extension raises `TypeError` for a non-string, and no rescue caught it, so it reached
      # `run` and stopped the server.
      def test_a_request_without_a_query_string_is_a_client_error
        core = Core.new(@state)

        [nil, 12345, ["MATCH (n) RETURN n"], { "q" => 1 }].each do |query|
          response = core.send(:handle_query, { "command" => "query", "query" => query })

          assert_equal(1, response["status"], "expected #{query.inspect} to be refused")
          assert_match(/no query string/, response["stderr"])
        end
      end

      def test_a_request_with_a_malformed_query_format_is_a_client_error
        # A real graph ensures the request reaches the extension and raises the `TypeError` this
        # test prevents, not a missing-graph error.
        core = with_graph
        core.instance_variable_set(:@manifest, {})

        # `false` is in the list because `|| "table"` would turn it into the default.
        [12345, ["json"], { "format" => "json" }, false].each do |format|
          response = core.send(
            :handle_query,
            { "command" => "query", "query" => "MATCH (n) RETURN n", "query_format" => format },
          )

          assert_equal(1, response["status"], "expected #{format.inspect} to be refused")
          assert_match(/no query format string/, response["stderr"])
        end
      end

      def test_a_request_without_a_query_format_uses_the_default
        core = with_graph
        core.instance_variable_set(:@manifest, {})

        response = core.send(:handle_query, { "command" => "query", "query" => "MATCH (c:Class) RETURN c.name" })

        assert_equal(0, response["status"])
      end

      # One entry that cannot be read must cost that entry alone. The rescue used to sit on the whole
      # method, so a broken entry ended the walk of its directory and hid every file after it.
      def test_the_manifest_walk_survives_an_unreadable_entry
        skip("symlinks are unavailable on this platform") if Gem.win_platform?

        # `File.mtime` raises `ENOENT` on a symlink that points nowhere.
        File.symlink(File.join(@workspace, "missing.rb"), File.join(@workspace, "broken.rb"))
        ["a.rb", "b.rb", "c.rb"].each { |name| File.write(File.join(@workspace, name), "class X; end\n") }

        manifest = {} #: Hash[String, Float]
        Core.new(@state).send(:collect_files, @workspace, manifest, [], [])

        assert_equal(["a.rb", "b.rb", "c.rb"], manifest.keys.map { |path| File.basename(path) }.sort)
      end

      def test_the_manifest_walk_survives_an_unreadable_directory
        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?
        skip("root reads every directory") if Process.uid.zero?

        File.write(File.join(@workspace, "visible.rb"), "class X; end\n")
        locked = File.join(@workspace, "locked")
        FileUtils.mkdir_p(locked)
        File.write(File.join(locked, "hidden.rb"), "class Y; end\n")
        File.chmod(0o000, locked)

        manifest = {} #: Hash[String, Float]
        Core.new(@state).send(:collect_files, @workspace, manifest, [], [])

        assert_equal(["visible.rb"], manifest.keys.map { |path| File.basename(path) })
      ensure
        File.chmod(0o700, locked) if locked && File.directory?(locked)
      end

      # A file the indexer could not read must stay stale, so the next request tries it again.
      # Recording its new mtime would call it fresh for the rest of the server's life.
      def test_a_file_that_fails_to_index_is_retried
        core = Core.new(@state)
        graph = Rubydex::Graph.configure_for_workspace(@workspace)
        core.instance_variable_set(:@graph, graph)

        # Invalid UTF-8 is the shape of failure the indexer reports as a `FileError`.
        unreadable = File.join(@workspace, "invalid_utf8.rb")
        File.binwrite(unreadable, "\xff\xfe not utf8 \xff\n")
        readable = File.join(@workspace, "fine.rb")
        File.write(readable, "class Fine; end\n")

        core.instance_variable_set(:@manifest, {})
        log, _err = capture_io { core.send(:refresh_if_stale) }

        assert_match(/index error: .*invalid_utf8\.rb/, log)

        manifest = core.instance_variable_get(:@manifest)
        assert(manifest.key?(readable), "a file that indexed must be recorded as fresh")
        refute(manifest.key?(unreadable), "a file that failed must stay stale, so it is retried")

        # And the retry really happens: a second walk still sees it, so it looks changed again.
        files, = core.send(:workspace_manifest)
        assert_includes(files.keys, unreadable)
      end

      # A directory that cannot be read right now contributes nothing to the walk. Its files are
      # still there, so treating them as deleted would erase a whole subtree from the graph over one
      # `chmod`, or over one moment during a checkout.
      def test_an_unreadable_directory_does_not_erase_its_subtree
        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?
        skip("root reads every directory") if Process.uid.zero?

        sub = File.join(@workspace, "sub")
        FileUtils.mkdir_p(sub)
        kept = File.join(sub, "kept.rb")
        File.write(kept, "class Kept; end\n")

        core = with_graph
        core.instance_variable_set(:@manifest, {})
        core.send(:refresh_if_stale)

        assert(core.instance_variable_get(:@manifest).key?(kept))
        refute_nil(core.instance_variable_get(:@graph)["Kept"], "the class must be indexed to start")

        File.chmod(0o000, sub)
        core.send(:refresh_if_stale)

        assert(
          core.instance_variable_get(:@manifest).key?(kept),
          "a file under an unreadable directory must keep its entry",
        )
        refute_nil(
          core.instance_variable_get(:@graph)["Kept"],
          "an unreadable directory must not delete the documents beneath it",
        )
      ensure
        File.chmod(0o700, sub) if sub && File.directory?(sub)
      end

      # A directory that is really gone must take its files with it, or the graph would answer with
      # classes that no longer exist.
      def test_a_deleted_directory_removes_its_subtree
        sub = File.join(@workspace, "gone")
        FileUtils.mkdir_p(sub)
        File.write(File.join(sub, "doomed.rb"), "class Doomed; end\n")

        core = with_graph
        core.instance_variable_set(:@manifest, {})
        core.send(:refresh_if_stale)
        refute_nil(core.instance_variable_get(:@graph)["Doomed"])

        FileUtils.rm_rf(sub)
        core.send(:refresh_if_stale)

        assert_empty(core.instance_variable_get(:@manifest).select { |path, _| path.start_with?(sub) })
        assert_nil(core.instance_variable_get(:@graph)["Doomed"], "a deleted file must leave the graph")
      end

      # The boot half of the same bug the refresh path fixes: a file the initial index could not read
      # must not start out fresh, or nothing would ever try it again.
      def test_a_file_that_fails_the_initial_index_is_not_fresh
        File.binwrite(File.join(@workspace, "invalid_utf8.rb"), "\xff\xfe not utf8 \xff\n")
        File.write(File.join(@workspace, "fine.rb"), "class Fine; end\n")

        graph, errors = Server.build_graph(workspace_path: @state.workspace_path)
        refute_empty(errors, "the unreadable file must be reported rather than discarded")

        core = Core.new(@state)
        core.instance_variable_set(:@graph, graph)
        manifest = nil #: Hash[String, Float]?
        capture_io { manifest = core.send(:initial_manifest, errors) }

        assert(manifest.key?(File.join(@state.workspace_path, "fine.rb")))
        refute(
          manifest.key?(File.join(@state.workspace_path, "invalid_utf8.rb")),
          "a file that failed the initial index must stay stale",
        )
      end

      # A file can fail the initial index and succeed on the retry, which puts a new document into a
      # graph that was already resolved. Serving that graph would answer without it, so the
      # reconciliation has to resolve again.
      def test_the_boot_reconciliation_resolves_what_it_indexes
        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?
        skip("root reads every file") if Process.uid.zero?

        File.write(File.join(@workspace, "parent.rb"), "class Parent; end\n")
        late = File.join(@workspace, "late.rb")
        File.write(late, "class Late < Parent; end\n")
        File.chmod(0o000, late) # unreadable while the initial index runs

        graph, errors = Server.build_graph(workspace_path: @state.workspace_path)
        refute_empty(errors, "the unreadable file must be reported")
        assert_nil(graph["Late"], "the file must be missing from the graph to begin with")

        File.chmod(0o600, late) # readable again by the time the reconciliation retries it

        core = Core.new(@state)
        core.instance_variable_set(:@graph, graph)
        manifest = nil #: Hash[String, Float]?
        capture_io { manifest = core.send(:initial_manifest, errors) }

        assert(manifest.key?(late), "the retry succeeded, so the file counts as fresh")
        refute_nil(graph["Late"], "a document the reconciliation added must be resolved")
        assert_includes(graph["Late"].ancestors.map(&:name), "Parent")
      ensure
        File.chmod(0o600, late) if late && File.exist?(late)
      end

      # Invalid Cypher is the caller's mistake, so it must return the parser's message, not an
      # internal error.
      def test_invalid_cypher_is_reported_as_a_user_error
        core = with_graph
        core.instance_variable_set(:@manifest, {})

        response = exchange_with(core, { "command" => "query", "query" => "NOT A QUERY" })

        assert_equal(1, response["status"])
        assert_match(/Cypher syntax error/, response["stderr"])
        refute_match(/internal error/, response["stderr"])
      end

      # The refresh runs inside the query path, but it answers for the server and not for the caller.
      # The rescue used to wrap it, so an `ArgumentError` from a refresh came back as a bad query and
      # blamed the user for a server fault.
      def test_a_failing_refresh_is_an_internal_error_and_the_server_survives
        core = with_graph
        core.instance_variable_set(:@manifest, {})
        core.define_singleton_method(:refresh_if_stale) { raise ArgumentError, "refresh exploded" }

        response = exchange_with(core, { "command" => "query", "query" => "MATCH (c:Class) RETURN c.name" })

        assert_equal(1, response["status"])
        assert_match(/internal error/, response["stderr"])
        assert_match(/refresh exploded/, response["stderr"])
        assert_match(/internal error: ArgumentError: refresh exploded/, @log)

        # The connection failed, the server did not. Once the fault clears, queries work again.
        core.singleton_class.send(:remove_method, :refresh_if_stale)
        again = exchange_with(core, { "command" => "query", "query" => "MATCH (c:Class) RETURN c.name" })

        assert_equal(0, again["status"])
      end

      # Attribution halves a failing batch until each bad file sits alone, so several good files
      # around one bad one must all survive, and only the bad one must stay stale.
      def test_index_isolates_the_bad_files_in_a_batch
        good = 8.times.map do |i|
          path = File.join(@workspace, "good#{i}.rb")
          File.write(path, "class Good#{i}; end\n")
          path
        end
        bad = 2.times.map do |i|
          path = File.join(@workspace, "bad#{i}.rb")
          File.binwrite(path, "\xff\xfe not utf8 \xff\n")
          path
        end

        core = with_graph
        indexed = nil #: Array[String]?
        capture_io { indexed = core.send(:index, (good + bad).sort) }

        assert_equal(good.sort, indexed.sort)
      end

      # An unreadable workspace root leaves nothing to attribute. The server still starts, and it
      # must still say why its graph is empty.
      def test_a_boot_error_is_logged_even_when_the_walk_found_nothing
        core = with_graph
        core.define_singleton_method(:workspace_manifest) { [{}, []] }

        manifest = nil #: Hash[String, Float]?
        log, _err = capture_io { manifest = core.send(:initial_manifest, ["FileError: Path `/nope` does not exist"]) }

        assert_empty(manifest)
        assert_match(/boot index error: FileError/, log)
      end

      # `ln -s .. sub/loop` used to walk the workspace into itself over and over. It recorded one
      # file 32 times under ever longer paths before the platform refused, and every one of those
      # phantom entries would look changed on every request for the life of the server.
      def test_the_walk_does_not_follow_a_symlink_cycle
        skip("symlinks are unavailable on this platform") if Gem.win_platform?

        FileUtils.mkdir_p(File.join(@workspace, "sub"))
        File.write(File.join(@workspace, "real.rb"), "class Real; end\n")
        File.symlink("..", File.join(@workspace, "sub", "loop"))

        files, = with_graph.send(:workspace_manifest)

        assert_equal([File.join(@state.workspace_path, "real.rb")], files.keys)
      end

      # Below the workspace root the Rust walker asks a `DirEntry` for its type, which never follows a
      # symlink. See `collect_files_does_not_follow_symlinked_directories` in `rust/rubydex/src`.
      def test_the_walk_does_not_follow_a_nested_symlinked_directory
        skip("symlinks are unavailable on this platform") if Gem.win_platform?

        outside = File.join(@workspace, "outside")
        FileUtils.mkdir_p(outside)
        File.write(File.join(outside, "hidden.rb"), "class Hidden; end\n")

        nested = File.join(@workspace, "nested")
        FileUtils.mkdir_p(nested)
        File.write(File.join(nested, "kept.rb"), "class Kept; end\n")
        File.symlink(outside, File.join(nested, "link"))

        files, = with_graph.send(:workspace_manifest)
        names = files.keys.map { |path| path.delete_prefix("#{@state.workspace_path}/") }.sort

        assert_equal(["nested/kept.rb", "outside/hidden.rb"], names)
        refute_includes(names, "nested/link/hidden.rb", "a nested symlinked directory must not be followed")
      end

      # A symlinked directory directly under the workspace is different: `Graph#workspace_paths` adds
      # it as an explicit root, and the Rust walker traverses an explicit root. See
      # `collect_files_indexes_symlinked_directory_roots`. The walk has to agree, or those files would
      # sit in the graph and never be refreshed.
      def test_the_walk_follows_a_top_level_symlinked_directory
        skip("symlinks are unavailable on this platform") if Gem.win_platform?

        target = Dir.mktmpdir("rdx-core-linked")
        File.write(File.join(target, "linked.rb"), "class Linked; end\n")
        File.symlink(target, File.join(@workspace, "vendor"))

        files, = with_graph.send(:workspace_manifest)
        names = files.keys.map { |path| path.delete_prefix("#{@state.workspace_path}/") }

        assert_equal(["vendor/linked.rb"], names)
        # Compared by suffix: the graph canonicalises the workspace path and the walk does not, which
        # is the same Ruby/Rust path parity question that Group D still owns.
        roots = Rubydex::Graph.configure_for_workspace(@state.workspace_path).workspace_paths
        assert(
          roots.any? { |path| path.end_with?("/vendor") },
          "the indexer treats it as an explicit root, which is why the walk follows it",
        )
      ensure
        FileUtils.rm_rf(target) if target
      end

      # A symlink to a file is indexed at its own path, not at the target's. See
      # `collect_files_indexes_symlinked_files_at_their_own_path`.
      def test_the_walk_records_a_symlinked_file_at_its_own_path
        skip("symlinks are unavailable on this platform") if Gem.win_platform?

        outside = Dir.mktmpdir("rdx-core-target")
        target = File.join(outside, "real.rb")
        File.write(target, "class Real; end\n")
        File.symlink(target, File.join(@workspace, "alias.rb"))

        files, = with_graph.send(:workspace_manifest)

        assert_equal([File.join(@state.workspace_path, "alias.rb")], files.keys)
      ensure
        FileUtils.rm_rf(outside) if outside
      end

      # `lstat` classifies a self-referential symlink as a plain entry, and then `File.mtime` follows
      # it and raises `ELOOP`. Nothing rescued that, so one such file made every query fail.
      def test_the_walk_survives_a_symlink_loop_file
        skip("symlinks are unavailable on this platform") if Gem.win_platform?

        loop_path = File.join(@workspace, "loop.rb")
        File.symlink("loop.rb", loop_path)
        File.write(File.join(@workspace, "good.rb"), "class Good; end\n")

        # The loop really does raise, which is what makes the rescue load-bearing.
        assert_raises(Errno::ELOOP) { File.mtime(loop_path) }

        files, = with_graph.send(:workspace_manifest)

        assert_equal([File.join(@state.workspace_path, "good.rb")], files.keys)
      end

      # A resource failure says nothing about one path. Skipping every entry would leave an empty
      # walk, and the refresh would read that as "every file was deleted" and erase the graph. The
      # error has to travel instead, and the manifest must not move.
      def test_a_resource_failure_does_not_empty_the_manifest
        File.write(File.join(@workspace, "kept.rb"), "class Kept; end\n")

        core = with_graph
        core.instance_variable_set(:@manifest, {})
        core.send(:refresh_if_stale)

        before = core.instance_variable_get(:@manifest)
        assert_equal(1, before.size)
        refute_nil(core.instance_variable_get(:@graph)["Kept"])

        core.define_singleton_method(:directory_to_walk?) { |_path, _top| raise Errno::EMFILE }

        assert_raises(Errno::EMFILE) { core.send(:refresh_if_stale) }

        assert_equal(before, core.instance_variable_get(:@manifest), "the manifest must not move")
        refute_nil(core.instance_variable_get(:@graph)["Kept"], "the graph must keep its documents")
      end

      # These two exist wherever Ruby runs, so the walk can always count on them. A resource failure
      # must never be in the list: swallowing one would empty the walk and erase the graph.
      def test_the_path_errors_hold_the_universal_names_and_no_resource_error
        assert_includes(Core::PATH_ERRORS, Errno::ENOENT)
        assert_includes(Core::PATH_ERRORS, Errno::EACCES)
        refute_includes(Core::PATH_ERRORS, Errno::EMFILE)
      end

      # The list is resolved by lookup so a platform missing an optional errno still loads this file.
      # The lookup would also hide a misspelled name, so the full resolution is pinned here, where
      # every one of these constants is guaranteed to exist.
      def test_every_path_error_name_resolves_on_a_posix_platform
        skip("the set of errno constants differs on this platform") if Gem.win_platform?

        assert_equal(
          Core::PATH_ERROR_NAMES.size,
          Core::PATH_ERRORS.size,
          "a name in PATH_ERROR_NAMES did not resolve, which usually means a typo",
        )
        assert_includes(Core::PATH_ERRORS, Errno::ELOOP)
      end

      # A directory can be readable and still not searchable, mode `0400`. `each_child` then lists its
      # names while every `lstat` is refused, so the walk sees nothing under it and the outer rescue
      # never fires. The files are still there, and treating the gap as a deletion erased them.
      def test_a_directory_that_cannot_be_searched_does_not_erase_its_subtree
        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?
        skip("root searches every directory") if Process.uid.zero?

        sub = File.join(@workspace, "sub")
        FileUtils.mkdir_p(sub)
        kept = File.join(sub, "kept.rb")
        File.write(kept, "class Kept; end\n")

        core = with_graph
        core.instance_variable_set(:@manifest, {})
        core.send(:refresh_if_stale)

        assert(core.instance_variable_get(:@manifest).key?(kept))
        refute_nil(core.instance_variable_get(:@graph)["Kept"])

        File.chmod(0o400, sub)

        # The shape this test exists for: listing works, and classifying each entry does not.
        assert_equal(["kept.rb"], Dir.each_child(sub).to_a)
        assert_raises(Errno::EACCES) { File.lstat(kept) }

        core.send(:refresh_if_stale)

        assert(
          core.instance_variable_get(:@manifest).key?(kept),
          "a file under an unsearchable directory must keep its entry",
        )
        refute_nil(
          core.instance_variable_get(:@graph)["Kept"],
          "an unsearchable directory must not delete the documents beneath it",
        )
      ensure
        File.chmod(0o700, sub) if sub && File.directory?(sub)
      end

      private

      # A core wired to a real graph for this workspace, which is what the refresh path needs.
      #: -> Core
      def with_graph
        core = Core.new(@state)
        core.instance_variable_set(:@graph, Rubydex::Graph.configure_for_workspace(@state.workspace_path))
        core
      end

      #: (Core core, Hash[String, untyped] payload, ?token: String?) -> Hash[untyped, untyped]
      def exchange_with(core, payload, token: nil)
        server_side, client_side = UNIXSocket.pair

        client = Thread.new do
          client_side.gets
          Frame.write(client_side, payload.merge("token" => token || @state.token))
          Frame.read_response(client_side, total_timeout: 10.0)
        ensure
          client_side.close
        end

        @log, _err = capture_io { core.send(:handle, server_side) }
        client.value
      ensure
        client&.join
      end
    end
  end
end
