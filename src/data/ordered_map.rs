use std::hash::Hash;

use super::hash_table::HashTable;

#[derive(Debug, Clone)]
pub struct OrderedMap<K, V> {
    index: HashTable<K, usize>,
    entries: Vec<Option<(K, V)>>,
    len: usize,
}

impl<K, V> Default for OrderedMap<K, V>
where
    K: Clone + Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> OrderedMap<K, V>
where
    K: Clone + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            index: HashTable::new(),
            entries: Vec::new(),
            len: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.index.contains_key(key)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        let idx = *self.index.get(key)?;
        self.entries
            .get(idx)
            .and_then(Option::as_ref)
            .map(|(_, value)| value)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        let idx = *self.index.get(key)?;
        self.entries
            .get_mut(idx)
            .and_then(Option::as_mut)
            .map(|(_, value)| value)
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(idx) = self.index.get(&key).copied() {
            if let Some(Some((_, existing))) = self.entries.get_mut(idx) {
                return Some(std::mem::replace(existing, value));
            }
        }
        let idx = self.entries.len();
        self.entries.push(Some((key.clone(), value)));
        self.index.insert(key, idx);
        self.len += 1;
        None
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let idx = self.index.remove(key)?;
        let removed = self.entries.get_mut(idx)?.take();
        if let Some((_, value)) = removed {
            self.len = self.len.saturating_sub(1);
            Some(value)
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.entries
            .iter()
            .filter_map(|entry| entry.as_ref().map(|(key, value)| (key, value)))
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.iter().map(|(key, _)| key)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, value)| value)
    }
}

#[cfg(test)]
mod tests {
    use super::OrderedMap;

    #[test]
    fn ordered_map_preserves_insert_order() {
        let mut map = OrderedMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);
        let keys = map.keys().copied().collect::<Vec<_>>();
        assert_eq!(keys, vec!["a", "b", "c"]);
    }

    #[test]
    fn ordered_map_remove_keeps_relative_order() {
        let mut map = OrderedMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);
        assert_eq!(map.remove(&"b"), Some(2));
        let keys = map.keys().copied().collect::<Vec<_>>();
        assert_eq!(keys, vec!["a", "c"]);
    }
}
