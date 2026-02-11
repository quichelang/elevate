use std::hash::Hash;

use super::ordered_map::OrderedMap;

#[derive(Debug, Clone)]
pub struct OrderedSet<T> {
    inner: OrderedMap<T, ()>,
}

impl<T> Default for OrderedSet<T>
where
    T: Clone + Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> OrderedSet<T>
where
    T: Clone + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            inner: OrderedMap::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value, ()).is_none()
    }

    pub fn contains(&self, value: &T) -> bool {
        self.inner.contains_key(value)
    }

    pub fn remove(&mut self, value: &T) -> bool {
        self.inner.remove(value).is_some()
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.keys()
    }
}

#[cfg(test)]
mod tests {
    use super::OrderedSet;

    #[test]
    fn ordered_set_deduplicates() {
        let mut set = OrderedSet::new();
        assert!(set.insert("a"));
        assert!(!set.insert("a"));
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn ordered_set_preserves_order() {
        let mut set = OrderedSet::new();
        set.insert("x");
        set.insert("y");
        set.insert("z");
        assert_eq!(set.iter().copied().collect::<Vec<_>>(), vec!["x", "y", "z"]);
    }
}
