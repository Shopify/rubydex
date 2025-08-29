use crate::timer::Timer;

#[allow(clippy::cast_possible_truncation)]
fn seconds(microseconds: u128) -> String {
    format!("{:0>5.2}s", f64::from(microseconds as u32) / 1_000_000.0)
}

#[derive(Debug)]
pub struct Timers {
    add_uri: Timer,
    add_definition: Timer,
    clear_graph: Timer,
    collect_documents: Timer,
    extend_graph: Timer,
    index_documents: Timer,
    parse_ruby: Timer,
    visit_ruby: Timer,
}

impl Timers {
    #[must_use]
    pub fn new() -> Self {
        Self {
            add_uri: Timer::new(),
            add_definition: Timer::new(),
            clear_graph: Timer::new(),
            collect_documents: Timer::new(),
            extend_graph: Timer::new(),
            index_documents: Timer::new(),
            parse_ruby: Timer::new(),
            visit_ruby: Timer::new(),
        }
    }

    #[must_use]
    pub fn add_uri(&mut self) -> &mut Timer {
        &mut self.add_uri
    }

    #[must_use]
    pub fn add_definition(&mut self) -> &mut Timer {
        &mut self.add_definition
    }

    #[must_use]
    pub fn clear_graph(&mut self) -> &mut Timer {
        &mut self.clear_graph
    }

    #[must_use]
    pub fn collect_documents(&mut self) -> &mut Timer {
        &mut self.collect_documents
    }

    #[must_use]
    pub fn extend_graph(&mut self) -> &mut Timer {
        &mut self.extend_graph
    }

    #[must_use]
    pub fn index_documents(&mut self) -> &mut Timer {
        &mut self.index_documents
    }

    #[must_use]
    pub fn parse_ruby(&mut self) -> &mut Timer {
        &mut self.parse_ruby
    }

    #[must_use]
    pub fn visit_ruby(&mut self) -> &mut Timer {
        &mut self.visit_ruby
    }

    pub fn print(&self) {
        eprintln!("\nTimers:");
        eprintln!("  ├─ {}\tcollect_documents", seconds(self.collect_documents.elapsed()));
        eprintln!("  ├─ {}\tindex_documents", seconds(self.index_documents.elapsed()));
        eprintln!("  │  ├─ {}\t*parse_ruby", seconds(self.parse_ruby.elapsed()));
        eprintln!("  │  ├─ {}\t*visit_ruby", seconds(self.visit_ruby.elapsed()));
        eprintln!("  │  ├─ {}\t*add_uri", seconds(self.add_uri.elapsed()));
        eprintln!("  │  ├─ {}\t*add_definition", seconds(self.add_definition.elapsed()));
        eprintln!("  |  └─ {}\textend_graph", seconds(self.extend_graph.elapsed()));
        eprintln!("  └─ {}\tclear_graph", seconds(self.clear_graph.elapsed()));
    }

    pub fn add(&mut self, other: &Timers) {
        self.add_uri.add(other.add_uri.elapsed());
        self.add_definition.add(other.add_definition.elapsed());
        self.clear_graph.add(other.clear_graph.elapsed());
        self.collect_documents.add(other.collect_documents.elapsed());
        self.extend_graph.add(other.extend_graph.elapsed());
        self.index_documents.add(other.index_documents.elapsed());
        self.parse_ruby.add(other.parse_ruby.elapsed());
        self.visit_ruby.add(other.visit_ruby.elapsed());
    }
}

impl Default for Timers {
    fn default() -> Self {
        Self::new()
    }
}
