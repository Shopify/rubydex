use ruby_prism::Visit;
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug)]
struct Visitor {
    repository: Repository,
}

impl Visitor {
    fn new(repository: Repository) -> Self {
        Self {
            repository: repository,
        }
    }
}

impl Visit<'_> for Visitor {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        // store info about class node in index
        // Get the class name as a string from the constant path
        let class_name = String::from_utf8_lossy(node.name().as_slice()).to_string();
        println!("{}", class_name);

        let entry = Entry {
            name: class_name,
            file_path: "testpath".to_string(),
        };
        self.repository.add_entry(entry);

        ruby_prism::visit_class_node(self, node);
    }
}

#[derive(Debug)]
pub struct Repository {
    pub entries: HashMap<String, Entry>,
}

impl Repository {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
    pub fn add_entry(&mut self, entry: Entry) {
        self.entries.insert(entry.name.clone(), entry);
    }

    pub fn get_entry(&self, key: &str) -> Option<&Entry> {
        self.entries.get(key)
    }
}

#[derive(Clone, Debug)]
pub struct Entry {
    pub name: String,
    pub file_path: String,
}

impl Entry {
    pub fn new(name: String, file_path: String) -> Self {
        Self { name, file_path }
    }
}

pub fn index_all(file_paths: Vec<String>) -> () {
    let repository = Repository::new();
    let mut visitor = Visitor::new(repository);
    for file_path in file_paths {
        println!("{}", file_path);
        let source = match std::fs::read_to_string(&file_path) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {}: {}", file_path, err);
                return;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        visitor.visit(&result.node());
    }
}
