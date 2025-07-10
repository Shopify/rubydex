#[derive(Debug, Clone)]
#[repr(C)]
pub enum Declaration {
    Class(ClassDeclaration),
    Module(ModuleDeclaration),
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Location {
    start_offset: u32,
    end_offset: u32,
}

impl Location {
    pub const fn new(start_offset: u32, end_offset: u32) -> Self {
        Self {
            start_offset,
            end_offset,
        }
    }

    pub fn from_prism_location(prism_location: ruby_prism::Location) -> Self {
        Self::from_offsets(prism_location.start_offset(), prism_location.end_offset())
    }

    pub fn from_offsets(start_offset: usize, end_offset: usize) -> Self {
        Self {
            start_offset: start_offset.try_into().unwrap(),
            end_offset: end_offset.try_into().unwrap(),
        }
    }

    pub fn start_offset(&self) -> usize {
        self.start_offset.try_into().unwrap()
    }

    pub fn end_offset(&self) -> usize {
        self.end_offset.try_into().unwrap()
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct ClassDeclaration {
    location: Location,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct ModuleDeclaration {
    location: Location,
}

impl ClassDeclaration {
    pub const fn new(location: Location) -> Self {
        Self { location }
    }
}

impl ModuleDeclaration {
    pub const fn new(location: Location) -> Self {
        Self { location }
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Class(class_decl) => {
                let location = &class_decl.location;
                write!(f, "Class({}->{})", location.start_offset(), location.end_offset())
            }
            Self::Module(module_decl) => {
                let location = &module_decl.location;
                write!(f, "Module({}->{})", location.start_offset(), location.end_offset())
            }
        }
    }
}
