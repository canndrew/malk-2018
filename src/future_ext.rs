use super::*;

pub trait FutureExt: Future {
    fn compat_context<D>(self, context: D) -> BoxSendFuture<Self::Item, Error>
    where
        Self: Send + 'static,
        Self::Item: Send + 'static,
        Self::Error: std::error::Error + Sync + Send + 'static,
        D: Display + Send + Sync + 'static;
}

impl<F> FutureExt for F
where
    F: Future,
{
    fn compat_context<D>(self, context: D) -> BoxSendFuture<Self::Item, Error>
    where
        F: Send + 'static,
        F::Item: Send + 'static,
        F::Error: std::error::Error + Sync + Send + 'static,
        D: Display + Send + Sync + 'static,
    {
        self
        .then(|res| {
            match res {
                Ok(x) => Ok(x),
                Err(e) => {
                    Err(Error::from_boxed_compat(Box::new(e)).context(context).into())
                },
            }
        })
        .into_send_boxed()
    }
}


