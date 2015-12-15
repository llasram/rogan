#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

#[macro_use] extern crate hyper;
extern crate serde;
extern crate serde_json;

use std::convert::From;
use std::error;
use std::fmt;
use std::result;
use hyper::client::{Client, IntoUrl, RequestBuilder};
use hyper::status::StatusCode;

header! {
    (XStarfighterAuthorization, "X-Starfighter-Authorization") => [String]
}

#[derive(Debug)]
pub enum Error {
    Hyper(hyper::Error),
    Json(serde_json::error::Error),
    Timeout(String),
    NotFound(String),
    BadRequest(String),
    Unauthorized(String),
    Unknown(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Hyper(ref err) => err.fmt(f),
            Error::Json(ref err) => err.fmt(f),
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
            Error::Hyper(ref err) => err.description(),
            Error::Json(ref err) => err.description(),
            Error::Timeout(ref err) => err,
            Error::NotFound(ref err) => err,
            Error::BadRequest(ref err) => err,
            Error::Unauthorized(ref err) => err,
            Error::Unknown(ref err) => err,
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Hyper(ref err) => Some(err),
            Error::Json(ref err) => Some(err),
            _ => None,
        }
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

pub type Result<T> = result::Result<T, Error>;

pub struct Api {
    token: String,
    client: Client,
}

#[derive(Deserialize, Debug)]
struct Heartbeat {
    ok: bool,
    error: Option<String>,
}

impl Api {
    pub fn new(token: &str) -> Self {
        Api { token: token.to_owned(), client: hyper::Client::new() }
    }

    fn get<U: IntoUrl>(&self, url: U) -> RequestBuilder {
        self.client.get(url).header(XStarfighterAuthorization(self.token.clone()))
    }

    fn base_url(&self) -> &'static str {
        "https://api.stockfighter.io/ob/api"
    }

    fn heartbeat_url(&self) -> String {
        format!("{}/heartbeat", self.base_url())
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.get(&self.heartbeat_url()).send());
        let status = res.status;
        let hb: Heartbeat = try!(serde_json::de::from_reader(res));
        let error = hb.error.unwrap_or_else(|| "<none>".to_owned());
        match (hb.ok, status) {
            (true, StatusCode::Ok) => Ok(()),
            _ => Err(Error::Unknown(error)),
        }
    }

    pub fn venue(&self, name: &str) -> Result<Venue> {
        Venue::new(self, name)
    }
}

pub struct Venue<'a> {
    api: &'a Api,
    name: String,
}

#[derive(Deserialize, Debug)]
struct VenueHeartbeat {
    ok: bool,
    error: Option<String>,
    venue: Option<String>,
}

impl<'a> Venue<'a> {
    fn new(api: &'a Api, name: &str) -> Result<Self> {
        let venue = Venue { api: api, name: name.to_owned() };
        try!(venue.heartbeat());
        Ok(venue)
    }

    fn base_url(&self) -> String {
        format!("{}/venues/{}", self.api.base_url(), self.name)
    }

    fn heartbeat_url(&self) -> String {
        format!("{}/heartbeat", self.base_url())
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.api.get(&self.heartbeat_url()).send());
        let status = res.status;
        let hb: VenueHeartbeat = try!(serde_json::de::from_reader(res));
        let error = hb.error.unwrap_or_else(|| "<none>".to_owned());
        match (hb.ok, status) {
            (true, StatusCode::Ok) => Ok(()),
            (false, StatusCode::InternalServerError) => Err(Error::Timeout(error)),
            (false, StatusCode::NotFound) => Err(Error::NotFound(error)),
            _ => Err(Error::Unknown(error)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_heartbeat() {
        let client = Api::new("37610898fa294d2f71e8c42072c39626d90a2c40");
        assert!(client.heartbeat().is_ok());
    }

    #[test]
    fn test_venue_heartbeat() {
        let client = Api::new("37610898fa294d2f71e8c42072c39626d90a2c40");
        assert!(client.venue("TESTEX").unwrap().heartbeat().is_ok());
    }
}
