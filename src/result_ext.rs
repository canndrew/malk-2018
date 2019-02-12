use super::*;

pub trait ResultExt<T, E> {
    fn compat_context<D>(self, context: D) -> Result<T, Error>
    where
        E: std::error::Error + Sync + Send + 'static,
        D: Display + Send + Sync + 'static;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn compat_context<D>(self, context: D) -> Result<T, Error>
    where
        E: std::error::Error + Sync + Send + 'static,
        D: Display + Send + Sync + 'static,
    {
        match self {
            Ok(x) => Ok(x),
            Err(e) => {
                Err(Error::from_boxed_compat(Box::new(e)).context(context).into())
            },
        }
    }
}

