struct Debug<T> {
    #[cfg(debug_assertions)]
    val: T,
    #[cfg(not(debug_assertions))]
    _priv: (),
}

impl<T> Try for Debug<T> {
    type Ok = T;
    type Error = ();

    fn into_result(self) -> Result<T, ()> {
        #[cfg(debug_assertions)]
        let ret = Ok(self.val);

        #[cfg(not(debug_assertions))]
        let ret = Err(());

        ret
    }

    fn from_error((): ()) -> Debug<T> {
        #[cfg(debug_assertions)]
        let ret = panic!("unexpected");

        #[cfg(not(debug_assertions))]
        let ret = Debug { _priv: () };

        ret
    }

    fn from_ok(val: T) -> Debug<T> {
        #[cfg(debug_assertions)]
        let ret = Debug { val }

        #[cfg(not(debug_assertions))]
        let ret = Debug { _priv: () };

        ret
    }
}

