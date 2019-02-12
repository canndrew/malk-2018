use super::*;

pub trait JsonSpec {
    type Output;

    fn parse_json(&self, json: Value) -> Result<Self::Output, Error>;
}

pub struct ExpectAny;

impl JsonSpec for ExpectAny {
    type Output = Value;

    fn parse_json(&self, json: Value) -> Result<Value, Error> {
        Ok(json)
    }
}

pub struct ExpectString;

impl JsonSpec for ExpectString {
    type Output = String;

    fn parse_json(&self, json: Value) -> Result<String, Error> {
        let ExpectString = self;
        match json {
            Value::String(s) => Ok(s),
            _ => bail!("expected string"),
        }
    }
}

pub struct ExpectU64;

impl JsonSpec for ExpectU64 {
    type Output = u64;

    fn parse_json(&self, json: Value) -> Result<u64, Error> {
        let ExpectU64 = self;
    }
}

pub struct ExpectArray<T>(pub T);

impl<T> JsonSpec for ExpectArray<T>
where
    T: JsonSpec,
{
    type Output = Vec<T::Output>;

    fn parse_json(&self, json: Value) -> Result<Vec<T::Output>, Error> {
        let ExpectArray(elem) = self;

        match json {
            Value::Array(vec) => {
                let mut ret = Vec::with_capacity(vec.len());
                for val in vec {
                    ret.push(elem.parse_json(val)?);
                }
                Ok(ret)
            },
            _ => bail!("expected array"),
        }
    }
}

pub trait FieldSpec {
    type Output;

    fn parse_json_field(&self, json: Option<Value>) -> Result<Self::Output, Error>;
}

pub struct Want<T>(pub T);
pub struct Need<T>(pub T);

impl<T> FieldSpec for Want<T>
where
    T: JsonSpec,
{
    type Output = Option<T::Output>;

    fn parse_json_field(&self, json: Option<Value>) -> Result<Option<T::Output>, Error> {
        let Want(spec) = self;
        match json {
            Some(inner) => Ok(Some(spec.parse_json(inner)?)),
            None => Ok(None),
        }
    }
}

impl<T> FieldSpec for Need<T>
where
    T: JsonSpec,
{
    type Output = T::Output;

    fn parse_json_field(&self, json: Option<Value>) -> Result<T::Output, Error> {
        let Need(spec) = self;
        match json {
            Some(inner) => Ok(spec.parse_json(inner)?),
            None => bail!("expected field"),
        }
    }
}

macro_rules! tuple_impl {
    ($($ty:ident,)*) => {
        impl<$($ty),*> JsonSpec for ($((&'static str, $ty),)*)
        where
            $($ty: FieldSpec,)*
        {
            type Output = ($($ty::Output,)*);

            fn parse_json(&self, json: Value) -> Result<Self::Output, Error> {
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
                    let (name, $ty) = $ty;
                    let sub_json = map.remove(*name);
                    let $ty = $ty.parse_json_field(sub_json)?;
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

