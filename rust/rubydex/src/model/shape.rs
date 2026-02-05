use crate::model::{
    definitions::{Definition, Mixin},
    ids::{DefinitionId, NameId, ReferenceId},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MixinKey {
    Include(NameId),
    Prepend(NameId),
    Extend(NameId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ShapeKey {
    Class {
        name_id: NameId,
        superclass_name_id: Option<NameId>,
        mixins: Vec<MixinKey>,
    },
    Module {
        name_id: NameId,
        mixins: Vec<MixinKey>,
    },
    SingletonClass {
        name_id: NameId,
        mixins: Vec<MixinKey>,
    },
    ConstantAlias {
        name_id: NameId,
        target_name_id: NameId,
    },
}

impl ShapeKey {
    #[must_use]
    pub fn name_id(&self) -> NameId {
        match self {
            ShapeKey::Class { name_id, .. }
            | ShapeKey::Module { name_id, .. }
            | ShapeKey::SingletonClass { name_id, .. }
            | ShapeKey::ConstantAlias { name_id, .. } => *name_id,
        }
    }
}

pub fn extract_shape_keys<'a, F>(
    definitions: impl IntoIterator<Item = (DefinitionId, &'a Definition)>,
    lookup_reference: F,
) -> Vec<ShapeKey>
where
    F: Fn(&ReferenceId) -> Option<NameId>,
{
    let mut keys: Vec<ShapeKey> = definitions
        .into_iter()
        .filter_map(|(_, def)| shape_key_for_definition(def, &lookup_reference))
        .collect();

    keys.sort_by_key(|k| *k.name_id());
    keys
}

fn shape_key_for_definition<F>(definition: &Definition, lookup_reference: &F) -> Option<ShapeKey>
where
    F: Fn(&ReferenceId) -> Option<NameId>,
{
    match definition {
        Definition::Class(class) => {
            let superclass_name_id = class.superclass_ref().and_then(lookup_reference);
            let mixins = extract_mixin_keys(class.mixins(), lookup_reference);

            Some(ShapeKey::Class {
                name_id: *class.name_id(),
                superclass_name_id,
                mixins,
            })
        }
        Definition::Module(module) => {
            let mixins = extract_mixin_keys(module.mixins(), lookup_reference);

            Some(ShapeKey::Module {
                name_id: *module.name_id(),
                mixins,
            })
        }
        Definition::SingletonClass(singleton) => {
            let mixins = extract_mixin_keys(singleton.mixins(), lookup_reference);

            Some(ShapeKey::SingletonClass {
                name_id: *singleton.name_id(),
                mixins,
            })
        }
        Definition::ConstantAlias(alias) => Some(ShapeKey::ConstantAlias {
            name_id: *alias.name_id(),
            target_name_id: *alias.target_name_id(),
        }),
        Definition::Constant(_)
        | Definition::Method(_)
        | Definition::AttrAccessor(_)
        | Definition::AttrReader(_)
        | Definition::AttrWriter(_)
        | Definition::GlobalVariable(_)
        | Definition::InstanceVariable(_)
        | Definition::ClassVariable(_)
        | Definition::MethodAlias(_)
        | Definition::GlobalVariableAlias(_) => None,
    }
}

fn extract_mixin_keys<F>(mixins: &[Mixin], lookup_reference: &F) -> Vec<MixinKey>
where
    F: Fn(&ReferenceId) -> Option<NameId>,
{
    mixins
        .iter()
        .filter_map(|mixin| {
            let ref_id = mixin.constant_reference_id();
            lookup_reference(ref_id).map(|name_id| match mixin {
                Mixin::Include(_) => MixinKey::Include(name_id),
                Mixin::Prepend(_) => MixinKey::Prepend(name_id),
                Mixin::Extend(_) => MixinKey::Extend(name_id),
            })
        })
        .collect()
}

#[must_use]
pub fn shapes_match(old_keys: &[ShapeKey], new_keys: &[ShapeKey]) -> bool {
    old_keys == new_keys
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shape_key_ordering() {
        let key1 = ShapeKey::Class {
            name_id: NameId::from("A"),
            superclass_name_id: None,
            mixins: vec![],
        };
        let key2 = ShapeKey::Class {
            name_id: NameId::from("B"),
            superclass_name_id: None,
            mixins: vec![],
        };

        assert_eq!(key1.name_id(), NameId::from("A"));
        assert_eq!(key2.name_id(), NameId::from("B"));
    }

    #[test]
    fn test_mixin_order_matters() {
        let mixins1 = vec![
            MixinKey::Include(NameId::from("A")),
            MixinKey::Include(NameId::from("B")),
        ];
        let mixins2 = vec![
            MixinKey::Include(NameId::from("B")),
            MixinKey::Include(NameId::from("A")),
        ];

        assert_ne!(mixins1, mixins2);
    }

    #[test]
    fn test_mixin_type_matters() {
        let include = MixinKey::Include(NameId::from("A"));
        let prepend = MixinKey::Prepend(NameId::from("A"));
        let extend = MixinKey::Extend(NameId::from("A"));

        assert_ne!(include, prepend);
        assert_ne!(include, extend);
        assert_ne!(prepend, extend);
    }
}
