use crate::comments::CommentData;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::BufWriter;

#[derive(Clone)]
pub enum Encoding {
    JSON,
    POSTCARD,
    MESSAGEPACK,
    BINCODE,
    CBOR,
    //PROST,
}

impl fmt::Display for Encoding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Encoding::JSON => write!(f, "json"),
            Encoding::POSTCARD => write!(f, "postcard"),
            Encoding::MESSAGEPACK => write!(f, "messagepack"),
            Encoding::BINCODE => write!(f, "bincode"),
            Encoding::CBOR => write!(f, "cbor"),
            //Encoding::PROST => write!(f, "prost"),
        }
    }
}

impl Encoding {
    fn suffix(&self) -> String {
        match self {
            Encoding::JSON => String::from("json"),
            Encoding::POSTCARD => String::from("postcard"),
            Encoding::MESSAGEPACK => String::from("msgpack"),
            Encoding::BINCODE => String::from("bincode"),
            Encoding::CBOR => String::from("cbor"),
            //Encoding::PROST => String::from("prost"),
        }
    }
}

pub fn serialize_and_write(encoding: Encoding, data: &HashMap<String, CommentData>) {
    let path = format!("tmp/{}.{}", encoding.to_string(), encoding.suffix());
    let f = File::create(&path).expect("Unable to create file at '{path}'");
    match encoding {
        Encoding::JSON => {
            let bw = BufWriter::new(f);
            if serde_json::ser::to_writer(bw, &data).is_ok() {
                println!("Successfully written model to '{path}'");
            } else {
                eprintln!("Error writting model to '{path}'");
            }
        }
        Encoding::POSTCARD => {
            let serialized = postcard::to_allocvec(data).unwrap();
            let result = fs::write(path.clone(), &serialized);
            match result {
                Ok(()) => println!("Successfully written model to '{path}'"),
                Err(error) => eprintln!("Error writting model to '{path}'"),
            }
        }
        Encoding::MESSAGEPACK => {
            let serialized = rmp_serde::to_vec(data).unwrap();
            let result = fs::write(path.clone(), &serialized);
            match result {
                Ok(()) => println!("Successfully written model to '{path}'"),
                Err(error) => eprintln!("Error writting model to '{path}'"),
            }
        }
        Encoding::BINCODE => {
            let serialized = bincode::serialize(data).unwrap();
            let result = fs::write(path.clone(), &serialized);
            match result {
                Ok(()) => println!("Successfully written model to '{path}'"),
                Err(error) => eprintln!("Error writting model to '{path}'"),
            }
        }
        Encoding::CBOR => {
            let mut serialized = Vec::new();
            ciborium::into_writer(data, &mut serialized).unwrap();

            let result = fs::write(path.clone(), &serialized);
            match result {
                Ok(()) => println!("Successfully written model to '{path}'"),
                Err(error) => eprintln!("Error writting model to '{path}'"),
            }
        }
    }
}

pub fn deserialize_from_file(encoding: Encoding) -> Result<HashMap<String, CommentData>, Box<dyn std::error::Error>> {
    let path = format!("tmp/{}.{}", encoding.to_string(), encoding.suffix());
    match encoding {
        Encoding::JSON => {
            println!("{}", path);
            let data = fs::read(&path)?;
            let deserialized: HashMap<String, CommentData> = serde_json::from_slice(&data)?;
            println!("succesfully deserialized {}!", encoding.to_string());
            return Ok(deserialized);
        }
        Encoding::POSTCARD => {
            let data = fs::read(&path)?;
            let deserialized: HashMap<String, CommentData> = postcard::from_bytes(&data)?;
            println!("succesfully deserialized {}!", encoding.to_string());
            return Ok(deserialized);
        }
        Encoding::MESSAGEPACK => {
            let data = fs::read(&path)?;
            let deserialized: HashMap<String, CommentData> = rmp_serde::from_slice(&data)?;
            println!("succesfully deserialized {}!", encoding.to_string());
            return Ok(deserialized);
        }
        Encoding::BINCODE => {
            let data = fs::read(&path)?;
            let deserialized: HashMap<String, CommentData> = bincode::deserialize(&data)?;
            println!("succesfully deserialized {}!", encoding.to_string());
            return Ok(deserialized);
        }
        Encoding::CBOR => {
            let data = fs::read(&path)?;
            let deserialized: HashMap<String, CommentData> = ciborium::from_reader(&data[..])?;
            println!("succesfully deserialized {}!", encoding.to_string());
            return Ok(deserialized);
        }
    }
}
