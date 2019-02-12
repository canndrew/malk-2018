use super::*;

pub trait FromJson: Sized {
    fn from_json(json: Value) -> Result<Self, Error>;
}

pub trait ToJson: Sized {
    fn to_json(self) -> Value;
}

impl FromJson for u64 {
    fn from_json(json: Value) -> Result<u64, Error> {
        match json {
            Value::Number(n) => match n.as_u64() {
                Some(n) => Ok(n),
                None => bail!("number out of range"),
            }
            _ => bail!("expected string"),
        }
    }
}

impl FromJson for usize {
    fn from_json(json: Value) -> Result<usize, Error> {
        match json {
            Value::Number(n) => match n.as_u64() {
                Some(n) => match usize::try_from(n) {
                    Ok(n) => Ok(n),
                    Err(..) => bail!("number out of range"),
                }
                None => bail!("number out of range"),
            }
            _ => bail!("expected string"),
        }
    }
}

impl FromJson for String {
    fn from_json(json: Value) -> Result<String, Error> {
        match json {
            Value::String(s) => Ok(s),
            _ => bail!("expected string"),
        }
    }
}

impl FromJson for Value {
    fn from_json(json: Value) -> Result<Value, Error> {
        Ok(json)
    }
}

impl<T> ToJson for Vec<T>
where
    T: ToJson,
{
    fn to_json(self) -> Value {
        let mut ret = Vec::with_capacity(self.len());
        for val in self {
            let val = val.to_json();
            ret.push(val);
        }
        Value::Array(ret)
    }
}

impl<T> FromJson for Vec<T>
where
    T: FromJson,
{
    fn from_json(json: Value) -> Result<Vec<T>, Error> {
        let vec = match json {
            Value::Array(vec) => vec,
            _ => bail!("expected array"),
        };
        let mut ret = Vec::with_capacity(vec.len());
        for val in vec {
            let val = T::from_json(val)?;
            ret.push(val);
        }
        Ok(ret)
    }
}

