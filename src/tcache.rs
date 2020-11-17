use lru_time_cache::LruCache;
use crate::translation::Translation;
use std::sync::Arc;
use blake2::{Blake2s, Digest};
use std::time::Duration;
use std::sync::Mutex;

type Key = [u8; 32];

pub struct TranslationCache {
    backing: Mutex<LruCache<Key, Arc<Translation>>>,
}

#[derive(Debug, Clone)]
pub struct TranslationCacheConfig {
    pub size: usize,
    pub ttl: Duration,
}

impl TranslationCache {
    pub fn new(config: TranslationCacheConfig) -> TranslationCache {
        TranslationCache {
            backing: Mutex::new(LruCache::with_expiry_duration_and_capacity(config.ttl, config.size)),
        }
    }

    pub fn get(&self, k: &Key) -> Option<Arc<Translation>> {
        self.backing.lock().unwrap().get(k).cloned()
    }

    pub fn insert(&self, k: Key, t: Arc<Translation>) {
        self.backing.lock().unwrap().insert(k, t);
    }
}

pub fn hash(code: &[u8]) -> Key {
    let mut hasher = Blake2s::new();
    hasher.update(code);
    Key::from(hasher.finalize())
}
