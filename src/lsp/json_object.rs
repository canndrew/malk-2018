use super::*;

pub fn parse_object<S: ObjectSpec>(json: Value, spec: S) -> Result<S::Output, Error> {
    spec.parse_object(json)
}

pub struct Want<T> {
    name: &'static str,
    _ph: PhantomData<T>,
}

pub struct Need<T> {
    name: &'static str,
    _ph: PhantomData<T>,
}

pub fn want<T>(name: &'static str) -> Want<T> {
    Want {
        name,
        _ph: PhantomData,
    }
}

pub fn need<T>(name: &'static str) -> Need<T> {
    Need {
        name,
        _ph: PhantomData,
    }
}

pub trait FieldSpec {
    type Output;

    fn name(&self) -> &str;
    fn parse_field(&self, field: Option<Value>) -> Result<Self::Output, Error>;
}

impl<T> FieldSpec for Want<T>
where
    T: FromJson,
{
    type Output = Option<T>;

    fn name(&self) -> &str {
        self.name
    }

    fn parse_field(&self, field: Option<Value>) -> Result<Option<T>, Error> {
        match field {
            Some(field) => Ok(Some(T::from_json(field)?)),
            None => Ok(None),
        }
    }
}

impl<T> FieldSpec for Need<T>
where
    T: FromJson,
{
    type Output = T;

    fn name(&self) -> &str {
        self.name
    }

    fn parse_field(&self, field: Option<Value>) -> Result<T, Error> {
        match field {
            Some(field) => Ok(T::from_json(field)?),
            None => bail!("missing field {}", self.name),
        }
    }
}

pub trait ObjectSpec {
    type Output;

    fn parse_object(&self, json: Value) -> Result<Self::Output, Error>;
}

macro_rules! tuple_impl {
    ($($ty:ident,)*) => {
        impl<$($ty),*> ObjectSpec for ($($ty,)*)
        where
            $($ty: FieldSpec,)*
        {
            type Output = ($($ty::Output,)*);

            fn parse_object(&self, json: Value) -> Result<Self::Output, Error> {
                #![allow(non_snake_case)]
                #![allow(unused_assignments)]
                #![allow(unused_mut)]
                #![allow(unused_variables)]

                let mut map = match json {
                    Value::Object(map) => map,
                    _ => bail!("expected object"),
                };

                let ($($ty,)*) = self;

                $(
                    let name = $ty.name();
                    let sub_json = map.remove(name);
                    let $ty = $ty.parse_field(sub_json)?;
                )*

                Ok(($($ty,)*))
            }
        }
    }
}

tuple_impl!(T0,);
tuple_impl!(T0,T1,);
tuple_impl!(T0,T1,T2,);
tuple_impl!(T0,T1,T2,T3,);
tuple_impl!(T0,T1,T2,T3,T4,);
tuple_impl!(T0,T1,T2,T3,T4,T5,);
tuple_impl!(T0,T1,T2,T3,T4,T5,T6,);
tuple_impl!(T0,T1,T2,T3,T4,T5,T6,T7,);

