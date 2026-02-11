use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Bucket<K, V> {
    Empty,
    Tombstone,
    Occupied { hash: u64, key: K, value: V },
}

#[derive(Debug, Clone)]
pub struct HashTable<K, V> {
    buckets: Vec<Bucket<K, V>>,
    len: usize,
    tombstones: usize,
}

impl<K, V> Default for HashTable<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> HashTable<K, V> {
    pub fn new() -> Self {
        Self::with_capacity(8)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let size = capacity.max(8).next_power_of_two();
        Self {
            buckets: std::iter::repeat_with(|| Bucket::Empty).take(size).collect(),
            len: 0,
            tombstones: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl<K, V> HashTable<K, V>
where
    K: Eq + Hash,
{
    pub fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        let hash = hash_key(key);
        let index = self.find_bucket_index(key, hash)?;
        match self.buckets.get(index) {
            Some(Bucket::Occupied { value, .. }) => Some(value),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        let hash = hash_key(key);
        let index = self.find_bucket_index(key, hash)?;
        match self.buckets.get_mut(index) {
            Some(Bucket::Occupied { value, .. }) => Some(value),
            _ => None,
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.ensure_capacity();
        let hash = hash_key(&key);
        let slot = self.find_insertion_slot(&key, hash);
        match &mut self.buckets[slot] {
            Bucket::Occupied {
                key: existing_key,
                value: existing_value,
                ..
            } if existing_key == &key => Some(std::mem::replace(existing_value, value)),
            Bucket::Tombstone => {
                self.tombstones = self.tombstones.saturating_sub(1);
                self.len += 1;
                self.buckets[slot] = Bucket::Occupied { hash, key, value };
                None
            }
            Bucket::Empty => {
                self.len += 1;
                self.buckets[slot] = Bucket::Occupied { hash, key, value };
                None
            }
            Bucket::Occupied { .. } => None,
        }
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let hash = hash_key(key);
        let index = self.find_bucket_index(key, hash)?;
        let removed = std::mem::replace(&mut self.buckets[index], Bucket::Tombstone);
        if let Bucket::Occupied { value, .. } = removed {
            self.len = self.len.saturating_sub(1);
            self.tombstones += 1;
            Some(value)
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.buckets.iter().filter_map(|bucket| match bucket {
            Bucket::Occupied { key, value, .. } => Some((key, value)),
            Bucket::Empty | Bucket::Tombstone => None,
        })
    }

    fn ensure_capacity(&mut self) {
        let bucket_count = self.buckets.len();
        let load = (self.len + self.tombstones) * 100 / bucket_count;
        if load >= 70 || self.tombstones > self.len {
            self.rehash(bucket_count * 2);
        }
    }

    fn rehash(&mut self, new_capacity: usize) {
        let mut next = HashTable::<K, V>::with_capacity(new_capacity);
        for bucket in std::mem::take(&mut self.buckets) {
            if let Bucket::Occupied { key, value, .. } = bucket {
                next.insert(key, value);
            }
        }
        *self = next;
    }

    fn find_bucket_index(&self, key: &K, hash: u64) -> Option<usize> {
        let mask = self.buckets.len() - 1;
        let mut index = (hash as usize) & mask;
        loop {
            match &self.buckets[index] {
                Bucket::Empty => return None,
                Bucket::Tombstone => {}
                Bucket::Occupied {
                    hash: existing_hash,
                    key: existing_key,
                    ..
                } => {
                    if *existing_hash == hash && existing_key == key {
                        return Some(index);
                    }
                }
            }
            index = (index + 1) & mask;
        }
    }

    fn find_insertion_slot(&self, key: &K, hash: u64) -> usize {
        let mask = self.buckets.len() - 1;
        let mut index = (hash as usize) & mask;
        let mut first_tombstone = None::<usize>;
        loop {
            match &self.buckets[index] {
                Bucket::Empty => return first_tombstone.unwrap_or(index),
                Bucket::Tombstone => {
                    if first_tombstone.is_none() {
                        first_tombstone = Some(index);
                    }
                }
                Bucket::Occupied {
                    hash: existing_hash,
                    key: existing_key,
                    ..
                } => {
                    if *existing_hash == hash && existing_key == key {
                        return index;
                    }
                }
            }
            index = (index + 1) & mask;
        }
    }
}

fn hash_key<K: Hash>(key: &K) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::HashTable;

    #[test]
    fn hash_table_insert_get_remove_round_trip() {
        let mut table = HashTable::new();
        assert!(table.is_empty());
        assert_eq!(table.insert("a", 1), None);
        assert_eq!(table.insert("b", 2), None);
        assert_eq!(table.get(&"a"), Some(&1));
        assert_eq!(table.get(&"b"), Some(&2));
        assert_eq!(table.remove(&"a"), Some(1));
        assert_eq!(table.get(&"a"), None);
        assert_eq!(table.len(), 1);
    }

    #[test]
    fn hash_table_updates_existing_key() {
        let mut table = HashTable::new();
        assert_eq!(table.insert("k", 1), None);
        assert_eq!(table.insert("k", 7), Some(1));
        assert_eq!(table.get(&"k"), Some(&7));
    }
}
