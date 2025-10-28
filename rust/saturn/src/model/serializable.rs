use serde::{Deserialize, Serialize};

pub trait Serializable: Serialize + for<'a> Deserialize<'a> {
    /// Serializes the entity into the byte vector we store in the database
    ///
    /// # Panics
    ///
    /// This method will only panic if serialization fails, which should never happen
    #[must_use]
    fn serialize(&self) -> Vec<u8> {
        rmp_serde::to_vec(self).expect("Serializing document should always succeed")
    }

    /// # Panics
    ///
    /// This method will only panic if serialization fails, which should never happen unless there's corrupt data stored
    /// in the database
    #[must_use]
    fn deserialize(data: &[u8]) -> Self {
        rmp_serde::from_slice(data).expect("Deserializing document should always succeed")
    }
}
