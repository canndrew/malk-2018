use super::*;

pub struct Interner<T> {
    inner: Mutex<Inner<T>>,
}

struct Inner<T> {
    set: HashMap<Arc<T>, ()>,
    inserts_per_purge: usize,
    inserts_til_next_purge: usize,
}

impl<T: Hash + Eq> Interner<T> {
    pub fn new() -> Interner<T> {
        Interner {
            inner: Mutex::new(Inner {
                set: HashMap::new(),
                inserts_per_purge: 1,
                inserts_til_next_purge: 1,
            }),
        }
    }

    pub fn intern(&self, value: T) -> Arc<T> {
        let mut inner = unwrap!(self.inner.lock());

        inner.inserts_til_next_purge -= 1;
        if inner.inserts_til_next_purge == 0 {
            let length_of_set = inner.set.len();
            let mut num_purged = 0;
            inner.set.retain(|key, ()| {
                if Arc::strong_count(key) > 1 {
                    true
                } else {
                    num_purged += 1;
                    false
                }
            });

            let mut next_inserts_per_purge = inner.inserts_per_purge as u128;
            next_inserts_per_purge *= 1 + length_of_set as u128;
            next_inserts_per_purge /= 1 + 2 * num_purged;
            inner.inserts_per_purge = next_inserts_per_purge as usize;
            inner.inserts_til_next_purge = inner.inserts_per_purge;
        }

        match inner.set.get_key_value(&value) {
            Some((value, ())) => value.clone(),
            None => {
                let ret = Arc::new(value);
                inner.set.insert(ret.clone(), ());
                ret
            },
        }
    }
}

