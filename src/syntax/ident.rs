use super::*;
use crate::parser::Ast;

lazy_static! {
    static ref IDENTS: Interner<IdentInner> = Interner::new();
}

#[derive(Clone, Eq, Hash)]
pub struct Ident {
    inner: Arc<IdentInner>,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner.string, f)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner.string, f)
    }
}

#[derive(PartialEq, Eq, Debug)]
struct IdentInner {
    string: String,
    hash: u64,
}

impl Hash for IdentInner {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash.hash(hasher)
    }
}

impl Ident {
    pub fn as_str(&self) -> &str {
        &self.inner.string
    }

    pub fn get_hash(&self) -> u64 {
        self.inner.hash
    }

    pub fn new<S: Into<String>>(s: S) -> Ident {
        let string = s.into();
        let hash = {
            let mut hasher = DefaultHasher::new();
            hasher.write(string.as_bytes());
            hasher.finish()
        };
        Ident {
            inner: IDENTS.intern(IdentInner { string, hash }),
        }
    }
}

