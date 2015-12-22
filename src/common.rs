use std::error;
use std::fmt;
use std::io;
use std::io::Read;
use std::result;

use chrono;
use hyper;
use hyper::client::{Response};
use hyper::status::StatusCode;
use serde::Deserialize;
use serde_json;
use websocket;

header! {
    (XStarfighterAuthorization, "X-Starfighter-Authorization") => [String]
}

#[derive(Deserialize, Debug)]
pub struct ErrorResponse {
    pub ok: bool,
    pub error: String,
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Hyper(hyper::Error),
    Json(serde_json::error::Error),
    Chrono(chrono::format::ParseError),
    WebSocket(websocket::result::WebSocketError),
    Timeout(String),
    NotFound(String),
    BadRequest(String),
    Unauthorized(String),
    Unknown(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Io(ref err) => err.fmt(f),
            Error::Hyper(ref err) => err.fmt(f),
            Error::Json(ref err) => err.fmt(f),
            Error::Chrono(ref err) => err.fmt(f),
            Error::WebSocket(ref err) => err.fmt(f),
            Error::Timeout(ref err) => write!(f, "Timeout: {}", err),
            Error::NotFound(ref err) => write!(f, "NotFound: {}", err),
            Error::BadRequest(ref err) => write!(f, "BadRequest: {}", err),
            Error::Unauthorized(ref err) => write!(f, "Unauthorized: {}", err),
            Error::Unknown(ref err) => write!(f, "Unknown: {}", err),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Io(ref err) => err.description(),
            Error::Hyper(ref err) => err.description(),
            Error::Json(ref err) => err.description(),
            Error::Chrono(ref err) => err.description(),
            Error::WebSocket(ref err) => err.description(),
            Error::Timeout(ref err) => err,
            Error::NotFound(ref err) => err,
            Error::BadRequest(ref err) => err,
            Error::Unauthorized(ref err) => err,
            Error::Unknown(ref err) => err,
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Io(ref err) => Some(err),
            Error::Hyper(ref err) => Some(err),
            Error::Json(ref err) => Some(err),
            Error::Chrono(ref err) => Some(err),
            Error::WebSocket(ref err) => Some(err),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<hyper::Error> for Error {
    fn from(err: hyper::Error) -> Error {
        Error::Hyper(err)
    }
}

impl From<serde_json::error::Error> for Error {
    fn from(err: serde_json::error::Error) -> Error {
        Error::Json(err)
    }
}

impl From<chrono::format::ParseError> for Error {
    fn from(err: chrono::format::ParseError) -> Error {
        Error::Chrono(err)
    }
}

impl From<websocket::result::WebSocketError> for Error {
    fn from(err: websocket::result::WebSocketError) -> Error {
        Error::WebSocket(err)
    }
}

pub type Result<T> = result::Result<T, Error>;

fn parse_error(body: &str) -> Result<String> {
    let er: ErrorResponse = try!(serde_json::from_str(body));
    Ok(er.error)
}

pub fn parse_response<T>(mut res: Response) -> Result<T> where T: Deserialize {
    let status = res.status;
    let mut body = String::new();
    try!(res.read_to_string(&mut body));
    match status {
        StatusCode::Ok => serde_json::from_str(&body).map_err(|_| -> Error {
            match parse_error(&body) {
                Ok(error) => Error::Unknown(error),
                Err(err) => Error::from(err),
            }
        }),
        StatusCode::InternalServerError => Err(Error::Timeout(try!(parse_error(&body)))),
        StatusCode::NotFound => Err(Error::NotFound(try!(parse_error(&body)))),
        StatusCode::BadRequest => Err(Error::BadRequest(try!(parse_error(&body)))),
        StatusCode::Unauthorized => Err(Error::Unauthorized(try!(parse_error(&body)))),
        _ => Err(Error::Unknown(try!(parse_error(&body)))),
    }
}
