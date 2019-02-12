use priv_prelude::*;

#[allow(unused)]
pub struct Document {
    fname: PathBuf,
    source: String,
}

impl Document {
    pub fn open(fname: impl Into<PathBuf>) -> Document {
        let fname = fname.into();

        let mut file = match File::open(&fname) {
            Err(e) => panic!("error opening source file: {}", e),
            Ok(file) => file,
        };

        let source = {
            let mut source = Vec::new();
            match file.read_to_end(&mut source) {
                Ok(_) => (),
                Err(e) => panic!("error reading from source file: {}", e),
            }
            match String::from_utf8(source) {
                Ok(source) => source,
                Err(e) => panic!("source file is not valid utf8: {}", e),
            }
        };

        Document {
            fname,
            source,
        }
    }
}

