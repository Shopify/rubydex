use std::collections::HashMap;

use crate::model::ids::{ReferenceId, UriId};

#[derive(Debug, Default)]
pub struct ChangeSet {
    requires_full_resolution: bool,
    references_by_uri: HashMap<UriId, Vec<ReferenceId>>,
}

impl ChangeSet {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn record_update(&mut self, uri_id: UriId, reference_ids: Vec<ReferenceId>, has_semantic_changes: bool) {
        self.references_by_uri.insert(uri_id, reference_ids);
        if has_semantic_changes {
            self.requires_full_resolution = true;
        }
    }

    #[must_use]
    pub fn requires_full_resolution(&self) -> bool {
        self.requires_full_resolution
    }

    pub fn references_to_resolve(&self) -> impl Iterator<Item = &ReferenceId> {
        self.references_by_uri.values().flatten()
    }

    pub fn clear(&mut self) {
        self.requires_full_resolution = false;
        self.references_by_uri.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_change_set_does_not_require_full_resolution() {
        let change_set = ChangeSet::new();
        assert!(!change_set.requires_full_resolution());
    }

    #[test]
    fn record_update_without_semantic_changes() {
        let mut change_set = ChangeSet::new();
        let uri_id = UriId::from("file:///foo.rb");
        let ref_id = ReferenceId::from("ref1");

        change_set.record_update(uri_id, vec![ref_id], false);

        assert!(!change_set.requires_full_resolution());
        assert_eq!(change_set.references_to_resolve().count(), 1);
    }

    #[test]
    fn record_update_with_semantic_changes() {
        let mut change_set = ChangeSet::new();
        let uri_id = UriId::from("file:///foo.rb");
        let ref_id = ReferenceId::from("ref1");

        change_set.record_update(uri_id, vec![ref_id], true);

        assert!(change_set.requires_full_resolution());
    }

    #[test]
    fn multiple_updates_accumulate_references() {
        let mut change_set = ChangeSet::new();
        let uri1 = UriId::from("file:///foo.rb");
        let uri2 = UriId::from("file:///bar.rb");
        let ref1 = ReferenceId::from("ref1");
        let ref2 = ReferenceId::from("ref2");
        let ref3 = ReferenceId::from("ref3");

        change_set.record_update(uri1, vec![ref1, ref2], false);
        change_set.record_update(uri2, vec![ref3], false);

        assert_eq!(change_set.references_to_resolve().count(), 3);
    }

    #[test]
    fn updating_same_uri_replaces_references() {
        let mut change_set = ChangeSet::new();
        let uri = UriId::from("file:///foo.rb");
        let ref1 = ReferenceId::from("ref1");
        let ref2 = ReferenceId::from("ref2");

        change_set.record_update(uri, vec![ref1], false);
        change_set.record_update(uri, vec![ref2], false);

        assert_eq!(change_set.references_to_resolve().count(), 1);
    }

    #[test]
    fn semantic_changes_flag_is_sticky() {
        let mut change_set = ChangeSet::new();
        let uri1 = UriId::from("file:///foo.rb");
        let uri2 = UriId::from("file:///bar.rb");

        change_set.record_update(uri1, vec![], true);
        change_set.record_update(uri2, vec![], false);

        assert!(change_set.requires_full_resolution());
    }

    #[test]
    fn clear_resets_state() {
        let mut change_set = ChangeSet::new();
        let uri = UriId::from("file:///foo.rb");
        let ref_id = ReferenceId::from("ref1");

        change_set.record_update(uri, vec![ref_id], true);
        change_set.clear();

        assert!(!change_set.requires_full_resolution());
        assert_eq!(change_set.references_to_resolve().count(), 0);
    }
}
