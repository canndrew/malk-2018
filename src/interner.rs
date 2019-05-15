use super::*;

pub struct Interner<T> {
    set: HashMap<Rc<T>, ()>,
    inserts_per_purge: usize,
    inserts_til_next_purge: usize,
}

impl<T: Hash + Eq> Interner<T> {
    pub fn new() -> Interner<T> {
        Interner {
            set: HashMap::new(),
            inserts_per_purge: 1,
            inserts_til_next_purge: 1,
        }
    }

    pub fn insert(&mut self, value: T) -> Rc<T> {
        self.inserts_per_purge -= 1;
        if self.inserts_per_purge == 0 {
            let length_of_set = self.set.len();
            let mut num_purged = 0;
            self.set.retain(|key, ()| {
                if Rc::strong_count(key) > 1 {
                    true
                } else {
                    num_purged += 1;
                    false
                }
            });

            let mut next_inserts_per_purge = self.inserts_per_purge as u128;
            next_inserts_per_purge *= 1 + length_of_set as u128;
            next_inserts_per_purge /= 1 + 2 * num_purged;
            self.inserts_per_purge = next_inserts_per_purge as usize;
            self.inserts_til_next_purge = self.inserts_per_purge;
        }

        match self.set.get_key_value(&value) {
            Some((value, ())) => value.clone(),
            None => {
                let ret = Rc::new(value);
                self.set.insert(ret.clone(), ());
                ret
            },
        }
    }
}

