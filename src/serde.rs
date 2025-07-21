use crate::comments::CommentData;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::BufWriter;

// Include the generated protobuf code
mod proto_comments {
    include!(concat!(env!("OUT_DIR"), "/comments.rs"));
}

#[derive(Clone)]
pub enum Encoding {
    JSON,
    POSTCARD,
    MESSAGEPACK,
    BINCODE,
    CBOR,
    PROST,
}

impl fmt::Display for Encoding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Encoding::JSON => write!(f, "json"),
            Encoding::POSTCARD => write!(f, "postcard"),
            Encoding::MESSAGEPACK => write!(f, "messagepack"),
            Encoding::BINCODE => write!(f, "bincode"),
            Encoding::CBOR => write!(f, "cbor"),
            Encoding::PROST => write!(f, "prost"),
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
            Encoding::PROST => String::from("prost"),
        }
    }
}

pub fn serialize_and_write(encoding: Encoding, data: &HashMap<String, CommentData>) {
    let path = format!("tmp/{}.{}", encoding.to_string(), encoding.suffix());
    serialize_and_write_to_path(encoding, data, &path, false);
}

// Convert CommentData to ProtoCommentData
fn convert_to_proto_comment_data(comment: &CommentData) -> proto_comments::ProtoCommentData {
    proto_comments::ProtoCommentData {
        text: comment.text.clone(),
        entry_name: comment.entry_name.clone(),
    }
}

// Convert ProtoCommentData to CommentData
fn convert_from_proto_comment_data(proto_comment: &proto_comments::ProtoCommentData) -> CommentData {
    CommentData {
        text: proto_comment.text.clone(),
        entry_name: proto_comment.entry_name.clone(),
    }
}

// Convert HashMap<String, CommentData> to ProtoCommentCollection
fn convert_to_proto_collection(data: &HashMap<String, CommentData>) -> proto_comments::ProtoCommentCollection {
    let mut proto_comments = HashMap::new();
    for (key, comment) in data {
        proto_comments.insert(key.clone(), convert_to_proto_comment_data(comment));
    }
    proto_comments::ProtoCommentCollection {
        comments: proto_comments,
    }
}

// Convert ProtoCommentCollection to HashMap<String, CommentData>
fn convert_from_proto_collection(
    proto_collection: &proto_comments::ProtoCommentCollection,
) -> HashMap<String, CommentData> {
    let mut comments = HashMap::new();
    for (key, proto_comment) in &proto_collection.comments {
        comments.insert(key.clone(), convert_from_proto_comment_data(proto_comment));
    }
    comments
}

pub fn serialize_and_write_to_path(encoding: Encoding, data: &HashMap<String, CommentData>, path: &str, silent: bool) {
    let f = File::create(path).expect(&format!("Unable to create file at '{}'", path));
    match encoding {
        Encoding::JSON => {
            let bw = BufWriter::new(f);
            if serde_json::ser::to_writer(bw, &data).is_ok() {
                if !silent {
                    println!("Successfully written model to '{}'", path);
                }
            } else {
                eprintln!("Error writing model to '{}'", path);
            }
        }
        Encoding::POSTCARD => {
            let serialized = postcard::to_allocvec(data).unwrap();
            let result = std::fs::write(path, &serialized);
            match result {
                Ok(()) => {
                    if !silent {
                        println!("Successfully written model to '{}'", path);
                    }
                }
                Err(_) => eprintln!("Error writing model to '{}'", path),
            }
        }
        Encoding::MESSAGEPACK => {
            let serialized = rmp_serde::to_vec(data).unwrap();
            let result = std::fs::write(path, &serialized);
            match result {
                Ok(()) => {
                    if !silent {
                        println!("Successfully written model to '{}'", path);
                    }
                }
                Err(_) => eprintln!("Error writing model to '{}'", path),
            }
        }
        Encoding::BINCODE => {
            let serialized = bincode::serialize(data).unwrap();
            let result = std::fs::write(path, &serialized);
            match result {
                Ok(()) => {
                    if !silent {
                        println!("Successfully written model to '{}'", path);
                    }
                }
                Err(_) => eprintln!("Error writing model to '{}'", path),
            }
        }
        Encoding::CBOR => {
            let mut serialized = Vec::new();
            ciborium::into_writer(data, &mut serialized).unwrap();

            let result = std::fs::write(path, &serialized);
            match result {
                Ok(()) => {
                    if !silent {
                        println!("Successfully written model to '{}'", path);
                    }
                }
                Err(_) => eprintln!("Error writing model to '{}'", path),
            }
        }
        Encoding::PROST => {
            // Convert to protobuf format and serialize
            let proto_collection = convert_to_proto_collection(data);
            let serialized = prost::Message::encode_to_vec(&proto_collection);
            let result = std::fs::write(path, &serialized);
            match result {
                Ok(()) => {
                    if !silent {
                        println!("Successfully written model to '{}'", path);
                    }
                }
                Err(_) => eprintln!("Error writing model to '{}'", path),
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
        Encoding::PROST => {
            let data = fs::read(&path)?;
            let proto_collection: proto_comments::ProtoCommentCollection = prost::Message::decode(&data[..])?;
            let deserialized = convert_from_proto_collection(&proto_collection);
            println!("succesfully deserialized {}!", encoding.to_string());
            return Ok(deserialized);
        }
    }
}
