#[derive(Debug)]
pub enum Declaration {
    Class(ClassDeclaration),
    Module(ModuleDeclaration),
}

#[derive(Debug)]
pub struct Location {
    start_offset: usize,
    end_offset: usize,
}

impl Location {
    pub fn start_offset(&self) -> usize {
        self.start_offset
    }

    pub fn end_offset(&self) -> usize {
        self.end_offset
    }
}

#[derive(Debug)]
pub struct ClassDeclaration {
    location: Location,
}

#[derive(Debug)]
pub struct ModuleDeclaration {
    location: Location,
}

impl ClassDeclaration {
    pub fn new(location: ruby_prism::Location) -> Self {
        ClassDeclaration {
            location: Location {
                start_offset: location.start_offset(),
                end_offset: location.end_offset(),
            },
        }
    }
}

impl ModuleDeclaration {
    pub fn new(location: ruby_prism::Location) -> Self {
        ModuleDeclaration {
            location: Location {
                start_offset: location.start_offset(),
                end_offset: location.end_offset(),
            },
        }
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Class(class_decl) => {
                let location = &class_decl.location;
                write!(f, "Class({}->{})", location.start_offset(), location.end_offset())
            }
            Declaration::Module(module_decl) => {
                let location = &module_decl.location;
                write!(f, "Module({}->{})", location.start_offset(), location.end_offset())
            }
        }
    }
}
