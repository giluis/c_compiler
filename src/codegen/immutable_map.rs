use std::{collections::HashMap, ops::Deref};

struct ImmutableMap<K,T> (HashMap<K,T>)
where K: PartialEq + Eq + std::hash::Hash;


impl <Key,Value> ImmutableMap<Key,Value> 
where Key: PartialEq + Eq + std::hash::Hash{
    fn get(&self, key: &Key) -> Option<&Value> {
        self.0.get(key)
    }
}