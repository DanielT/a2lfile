use crate::{A2lObjectName, A2lObjectNameSetter};
use fnv::FnvBuildHasher;
use std::{
    cmp::Ordering,
    collections::HashMap,
    ops::{Index, IndexMut},
};

/// A list of named a2l items
///
/// An ItemList is an ordered collection of items, which additionally allows for
/// fast access to items by their name.
#[derive(Debug, Clone)]
pub struct ItemList<T: A2lObjectName> {
    // storage for items
    items: Vec<T>,
    // mapping from item name to index in the items vector
    map: HashMap<String, usize, FnvBuildHasher>,
}

impl<T: A2lObjectName> ItemList<T> {
    /// create a new ItemList
    pub fn new() -> Self {
        Self {
            items: vec![],
            map: HashMap::default(),
        }
    }

    /// create a new ItemList with a specified initial capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            items: Vec::with_capacity(capacity),
            map: HashMap::with_capacity_and_hasher(capacity, FnvBuildHasher::default()),
        }
    }

    /// push an item into the ItemList
    pub fn push(&mut self, value: T) {
        let index = self.items.len();
        let key = value.get_name().to_string();
        // the ItemList _shouldn't_ contain duplicate keys, but if it does, the map refers to the first one
        self.map.entry(key).or_insert(index);
        self.items.push(value);
    }

    /// pop an item from the ItemList
    pub fn pop(&mut self) -> Option<T> {
        let item = self.items.pop()?;
        // remove the item from the map
        self.map.remove(item.get_name());
        Some(item)
    }

    /// get an item by key
    pub fn get(&self, key: &str) -> Option<&T> {
        let index = self.map.get(key)?;
        Some(&self.items[*index])
    }

    /// get a mutable reference to an item by key
    pub fn get_mut(&mut self, key: &str) -> Option<&mut T> {
        let index = self.map.get(key)?;
        Some(&mut self.items[*index])
    }

    /// get the first item in the ItemList
    pub fn first(&self) -> Option<&T> {
        self.items.first()
    }

    /// get the last item in the ItemList
    pub fn last(&self) -> Option<&T> {
        self.items.last()
    }

    /// get the index of an item by key
    pub fn index(&self, key: &str) -> Option<usize> {
        self.map.get(key).copied()
    }

    /// Checks if the ItemList contains an item with the given key
    pub fn contains_key(&self, key: &str) -> bool {
        self.map.contains_key(key)
    }

    /// remove an item from the ItemList by key and return it
    pub fn swap_remove(&mut self, key: &str) -> Option<T> {
        let index = self.map.remove(key)?;
        let item = self.items.swap_remove(index);
        // the last item was swapped into the index, so we need to update the map
        self.map
            .insert(self.items[index].get_name().to_string(), index);
        Some(item)
    }

    /// remove an item from the ItemList by index and return it
    pub fn swap_remove_idx(&mut self, index: usize) -> Option<T> {
        if index < self.items.len() {
            let item = self.items.swap_remove(index);
            // remove the item from the map
            self.map.remove(item.get_name());
            // the last item was swapped into the index, so we need to update the map
            self.map
                .insert(self.items[index].get_name().to_string(), index);
            Some(item)
        } else {
            None
        }
    }

    /// Returns an iterator over references to the items in the ItemList
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items.iter()
    }

    /// Returns an iterator over mutable references to the items in the ItemList
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.items.iter_mut()
    }

    /// Returns an iterator over the keys in the ItemList
    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.map.keys()
    }

    /// Returns the number of items in the ItemList
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Checks if the ItemList is empty
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// extend the ItemList from an iterator
    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        let into_iter = iter.into_iter();
        let (low, _high) = into_iter.size_hint();
        self.items.reserve(low);
        self.map.reserve(low);
        for item in into_iter {
            self.push(item);
        }
    }

    /// remove all items from the ItemList
    pub fn clear(&mut self) {
        self.items.clear();
        self.map.clear();
    }

    /// trucate the ItemList to a specified length
    pub fn truncate(&mut self, len: usize) {
        if len < self.items.len() {
            self.items.truncate(len);
            self.map.clear();
            // rebuild the map after truncating
            for (idx, item) in self.items.iter().enumerate() {
                let key = item.get_name().to_string();
                self.map.insert(key, idx);
            }
        }
    }

    /// Selectively remove or retain items from the ItemList based on a predicate function
    pub fn retain<F>(&mut self, mut keep: F)
    where
        F: FnMut(&mut T) -> bool,
    {
        let mut items_new = Vec::with_capacity(self.items.len());
        std::mem::swap(&mut self.items, &mut items_new);
        self.map.clear();
        // re-insert only the items that are kept
        for mut item in items_new {
            if keep(&mut item) {
                let key = item.get_name().to_string();
                self.items.push(item);
                self.map.insert(key, self.items.len() - 1);
            }
        }
    }

    pub fn sort_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.items.sort_by(|a, b| cmp(a, b));
        self.map.clear();
        // rebuild the map after sorting
        for (idx, item) in self.items.iter().enumerate() {
            let key = item.get_name().to_string();
            self.map.insert(key, idx);
        }
    }
}

impl<T: A2lObjectName + A2lObjectNameSetter> ItemList<T> {
    /// Set the name of an item in the ItemList
    pub fn rename_item(&mut self, item_idx: usize, new_name: &str) {
        if item_idx < self.items.len() {
            let old_name = self.items[item_idx].get_name();
            self.map.remove(old_name);
            self.items[item_idx].set_name(new_name.to_string());
            self.map.insert(new_name.to_string(), item_idx);
        }
    }
}

impl<T> Default for ItemList<T>
where
    T: A2lObjectName,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T: A2lObjectName> Index<usize> for ItemList<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.items[index]
    }
}

impl<T: A2lObjectName> IndexMut<usize> for ItemList<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.items[index]
    }
}

impl<T: A2lObjectName> Index<&str> for ItemList<T> {
    type Output = T;

    fn index(&self, key: &str) -> &Self::Output {
        let index = self.map.get(key).unwrap();
        &self.items[*index]
    }
}

impl<T> FromIterator<T> for ItemList<T>
where
    T: A2lObjectName,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let into_iter = iter.into_iter();
        let (low, _high) = into_iter.size_hint();
        let mut item_list = ItemList::with_capacity(low);
        for item in into_iter {
            item_list.push(item);
        }
        item_list
    }
}

impl<T> IntoIterator for ItemList<T>
where
    T: A2lObjectName,
{
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a ItemList<T>
where
    T: A2lObjectName,
{
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut ItemList<T>
where
    T: A2lObjectName,
{
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter_mut()
    }
}

impl<T> PartialEq for ItemList<T>
where
    T: A2lObjectName + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
        // no need to compare the maps, as they are derived from the items
    }
}

#[macro_export]
macro_rules! itemlist {
    () => (
        $crate::ItemList::new()
    );
    ($($x:expr),+ $(,)?) => (
        {
            const CAP: usize = <[()]>::len(&[$({ stringify!($x); }),*]);
            let mut itemlist = $crate::ItemList::with_capacity(CAP);
            $(
                itemlist.push($x);
            )*
            itemlist
        }
    );
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    struct TestItem {
        name: String,
    }

    impl TestItem {
        fn new(name: &str) -> Self {
            Self {
                name: name.to_string(),
            }
        }
    }

    impl A2lObjectName for TestItem {
        fn get_name(&self) -> &str {
            &self.name
        }
    }

    impl A2lObjectNameSetter for TestItem {
        fn set_name(&mut self, name: String) {
            self.name = name;
        }
    }

    #[test]
    fn test_itemlist() {
        let mut itemlist = ItemList::new();
        assert!(itemlist.is_empty());
        assert_eq!(itemlist.len(), 0);
        assert_eq!(itemlist.first(), None);

        itemlist.push(TestItem::new("item1"));
        itemlist.push(TestItem::new("item2"));
        assert_eq!(itemlist.len(), 2);
        verify_itemlist(&itemlist);
        assert_eq!(itemlist.first().unwrap().get_name(), "item1");
        assert_eq!(itemlist.last().unwrap().get_name(), "item2");

        assert_eq!(itemlist[1].get_name(), "item2");
        assert_eq!(itemlist["item2"].get_name(), "item2");

        itemlist.push(TestItem::new("item3"));
        itemlist.push(TestItem::new("item4"));
        itemlist.swap_remove("item2").unwrap();
        verify_itemlist(&itemlist);
        assert_eq!(itemlist.len(), 3);
        assert_eq!(
            itemlist,
            itemlist![
                TestItem::new("item1"),
                TestItem::new("item4"),
                TestItem::new("item3")
            ]
        );

        itemlist.push(TestItem::new("item5"));
        itemlist.swap_remove_idx(1).unwrap();
        verify_itemlist(&itemlist);
        assert_eq!(itemlist.len(), 3);
        assert_eq!(
            itemlist,
            itemlist![
                TestItem::new("item1"),
                TestItem::new("item5"),
                TestItem::new("item3")
            ]
        );
        assert_eq!(itemlist.swap_remove_idx(99), None);

        itemlist.retain(|item| item.get_name() != "item1");
        verify_itemlist(&itemlist);
        assert_eq!(itemlist.len(), 2);
        assert_eq!(
            itemlist,
            itemlist![TestItem::new("item5"), TestItem::new("item3")]
        );

        let item = itemlist.pop().unwrap();
        assert_eq!(item.get_name(), "item3");

        itemlist.push(TestItem::new("item6"));
        itemlist.push(TestItem::new("item7"));
        assert_eq!(itemlist.len(), 3);
        itemlist.truncate(99);
        assert_eq!(itemlist.len(), 3);
        itemlist.truncate(2);
        verify_itemlist(&itemlist);
        assert_eq!(itemlist.len(), 2);
        assert_eq!(
            itemlist,
            itemlist![TestItem::new("item5"), TestItem::new("item6")]
        );

        itemlist.rename_item(1, "item8");
        verify_itemlist(&itemlist);
        assert_eq!(itemlist.len(), 2);
        assert_eq!(
            itemlist,
            itemlist![TestItem::new("item5"), TestItem::new("item8")]
        );
    }

    fn verify_itemlist(itemlist: &ItemList<TestItem>) {
        assert_eq!(itemlist.len(), itemlist.items.len());
        assert_eq!(itemlist.map.len(), itemlist.items.len());
        for (idx, item) in itemlist.iter().enumerate() {
            assert_eq!(idx, itemlist.index(item.get_name()).unwrap());
            assert_eq!(item.get_name(), itemlist[idx].get_name());
        }
    }
}
