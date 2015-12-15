#![feature(custom_derive, custom_attribute, plugin)]
#![plugin(serde_macros)]

extern crate chrono;
#[macro_use] extern crate hyper;
extern crate serde;
extern crate serde_json;

use std::convert::From;
use std::error;
use std::fmt;
use std::result;

use chrono::{DateTime, UTC};
use hyper::client::{Client, RequestBuilder};
use hyper::status::StatusCode;

header! {
    (XStarfighterAuthorization, "X-Starfighter-Authorization") => [String]
}

#[derive(Debug)]
pub enum Error {
    Hyper(hyper::Error),
    Json(serde_json::error::Error),
    Chrono(chrono::format::ParseError),
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
            Error::Chrono(ref err) => err.fmt(f),
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
            Error::Chrono(ref err) => err.description(),
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
            Error::Chrono(ref err) => Some(err),
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

impl From<chrono::format::ParseError> for Error {
    fn from(err: chrono::format::ParseError) -> Error {
        Error::Chrono(err)
    }
}

pub type Result<T> = result::Result<T, Error>;

static DEFAULT_API_URL: &'static str = "https://api.stockfighter.io/ob/api";

pub struct Api {
    url: String,
    token: String,
    client: Client,
}

impl fmt::Debug for Api {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Api {{ url: {:?}, token: {:?} }}", self.url, self.token)
    }
}

#[derive(Deserialize, Debug)]
struct HeartbeatResponse {
    ok: bool,
    error: Option<String>,
}

impl Api {
    pub fn new(token: &str) -> Self {
        Api {
            url: DEFAULT_API_URL.to_owned(),
            token: token.to_owned(),
            client: hyper::Client::new()
        }
    }

    fn url(&self, url: &str) -> String {
        format!("{}/{}", self.url, url)
    }

    fn get(&self, url: &str) -> RequestBuilder {
        let auth = XStarfighterAuthorization(self.token.clone());
        self.client.get(&self.url(url)).header(auth)
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.get("heartbeat").send());
        let status = res.status;
        let hb: HeartbeatResponse = try!(serde_json::de::from_reader(res));
        let error = hb.error.unwrap_or_else(|| "<none>".to_owned());
        match (status, hb.ok) {
            (StatusCode::Ok, true) => Ok(()),
            _ => Err(Error::Unknown(error)),
        }
    }

    pub fn venue(&self, name: &str) -> Result<Venue> {
        Venue::new(self, name)
    }
}

pub struct Venue<'a> {
    api: &'a Api,
    pub name: String,
}

impl<'a> fmt::Debug for Venue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Venue {{ name: {:?} }}", self.name)
    }
}

#[derive(Deserialize, Debug)]
struct VenueHeartbeatResponse {
    ok: bool,
    error: Option<String>,
    venue: Option<String>,
}

#[derive(Deserialize, Debug)]
struct VenueStockSymbol {
    symbol: String,
    name: String,
}

#[derive(Deserialize, Debug)]
struct VenueStocksResponse {
    ok: bool,
    error: Option<String>,
    symbols: Option<Vec<VenueStockSymbol>>,
}

impl<'a> Venue<'a> {
    fn new(api: &'a Api, name: &str) -> Result<Self> {
        let venue = Venue { api: api, name: name.to_owned() };
        try!(venue.heartbeat());
        Ok(venue)
    }

    fn url(&self, url: &str) -> String {
        format!("venues/{}/{}", self.name, url)
    }

    fn get(&self, url: &str) -> RequestBuilder {
        self.api.get(&self.url(url))
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.get("heartbeat").send());
        let status = res.status;
        let hb: VenueHeartbeatResponse = try!(serde_json::de::from_reader(res));
        let error = hb.error.unwrap_or_else(|| "<none>".to_owned());
        match (status, hb.ok) {
            (StatusCode::Ok, true) => Ok(()),
            (StatusCode::InternalServerError, false) => Err(Error::Timeout(error)),
            (StatusCode::NotFound, false) => Err(Error::NotFound(error)),
            _ => Err(Error::Unknown(error)),
        }
    }

    pub fn stocks(&self) -> Result<Vec<Stock>> {
        let res = try!(self.get("stocks").send());
        let status = res.status;
        let vs: VenueStocksResponse = try!(serde_json::de::from_reader(res));
        let error = vs.error.unwrap_or_else(|| "<none>".to_owned());
        match (status, vs.ok, vs.symbols) {
            (StatusCode::Ok, true, Some(symbols)) => {
                let stocks = symbols.into_iter().map(|stock| {
                    Stock { venue: self, symbol: stock.symbol, name: stock.name }
                }).collect::<Vec<_>>();
                Ok(stocks)
            },
            (StatusCode::NotFound, false, None) => Err(Error::NotFound(error)),
            _ => Err(Error::Unknown(error)),
        }
    }

    pub fn stock(&self, symbol: &str) -> Result<Stock> {
        let symbol = symbol.to_owned();
        let name = "<unknown>".to_owned();
        let stock = Stock { venue: self, symbol: symbol, name: name };
        Ok(stock)
    }
}

#[derive(Debug)]
pub struct Stock<'a> {
    pub venue: &'a Venue<'a>,
    pub symbol: String,
    pub name: String,
}

#[derive(Deserialize, Debug)]
pub struct StockOrder {
    pub price: u64,
    pub qty: u64,
    #[serde(rename="isBuy")]
    pub is_buy: bool,
}

#[derive(Deserialize, Debug)]
struct StockOrdersResponse {
    ok: bool,
    error: Option<String>,
    ts: Option<String>,
    venue: Option<String>,
    symbol: Option<String>,
    bids: Option<Vec<StockOrder>>,
    asks: Option<Vec<StockOrder>>,
}

impl<'a> Stock<'a> {
    fn url(&self) -> String {
        format!("stocks/{}", self.symbol)
    }

    fn get(&self) -> RequestBuilder {
        self.venue.get(&self.url())
    }

    pub fn orders(&self) -> Result<StockOrders> {
        let res = try!(self.get().send());
        let status = res.status;
        let so: StockOrdersResponse = try!(serde_json::de::from_reader(res));
        let error = so.error.unwrap_or_else(|| "<none>".to_owned());
        match (status, so.ok, so.ts, so.bids, so.asks) {
            (StatusCode::Ok, true, Some(ts), Some(bids), Some(asks)) => {
                let ts = try!(ts.parse::<DateTime<UTC>>());
                let orders = StockOrders { stock: self, ts: ts, bids: bids, asks: asks };
                Ok(orders)
            },
            (StatusCode::NotFound, false, None, None, None) => Err(Error::NotFound(error)),
            _ => Err(Error::Unknown(error)),
        }
    }
}

#[derive(Debug)]
pub struct StockOrders<'a> {
    pub stock: &'a Stock<'a>,
    pub ts: DateTime<UTC>,
    pub bids: Vec<StockOrder>,
    pub asks: Vec<StockOrder>,
}

#[cfg(test)]
mod tests {
    use super::*;

    static TOKEN: &'static str = "37610898fa294d2f71e8c42072c39626d90a2c40";

    #[test]
    fn test_heartbeat() {
        let client = Api::new(TOKEN);
        assert!(client.heartbeat().is_ok());
    }

    #[test]
    fn test_venue_heartbeat() {
        let client = Api::new(TOKEN);
        let venue = client.venue("TESTEX").unwrap();
        assert!(venue.heartbeat().is_ok());
    }

    #[test]
    fn test_venue_stocks() {
        let client = Api::new(TOKEN);
        let venue = client.venue("TESTEX").unwrap();
        let stocks = venue.stocks().unwrap();
        assert_eq!(1, stocks.len());
        assert_eq!("FOOBAR", stocks[0].symbol);
    }

    #[test]
    fn test_stock_orders() {
        let client = Api::new(TOKEN);
        let venue = client.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let orders = stock.orders().unwrap();
        assert!(0 < orders.bids.len());
        assert!(0 < orders.asks.len());
    }
}
