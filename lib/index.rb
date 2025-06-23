# frozen_string_literal: true

if Gem.win_platform?
  require "fiddle"
  kernel32 = Fiddle.dlopen("kernel32")
  load_library = Fiddle::Function.new(kernel32["LoadLibraryW"], [Fiddle::TYPE_VOIDP], Fiddle::TYPE_INT)
  dll_path = File.expand_path(File.join("index", "index.dll"), __dir__)

  abort "Failed to load index.dll" if load_library.call(dll_path.encode("utf-16le")).zero?
end

require "index/version"
require "index/index"

module Index
end
