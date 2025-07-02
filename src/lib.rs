pub mod c_interface;

#[derive(Debug)]
pub struct Repository {
    name_pool: Vec<String>,
}

impl Repository {
    pub fn new() -> Self {
        Repository { name_pool: Vec::new() }
    }

    pub fn index_all(&mut self, file_paths: Vec<String>) {
        for path in file_paths {
            self.name_pool.push(path);
        }
    }
}

impl Default for Repository {
    fn default() -> Self {
        Self::new()
    }
}
