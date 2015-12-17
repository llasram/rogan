#![feature(custom_derive, custom_attribute, plugin)]
#![plugin(serde_macros)]

extern crate chrono;
#[macro_use] extern crate hyper;
extern crate serde;
extern crate serde_json;

use std::convert::From;
use std::error;
use std::fmt;
use std::io;
use std::io::Read;
use std::result;
use std::str::FromStr;

use chrono::{DateTime, UTC};
use hyper::client::{Client, RequestBuilder, Response};
use hyper::method::Method;
use hyper::status::StatusCode;
use serde::Deserialize;

header! {
    (XStarfighterAuthorization, "X-Starfighter-Authorization") => [String]
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
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
            Error::Io(ref err) => err.fmt(f),
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
            Error::Io(ref err) => err.description(),
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
            Error::Io(ref err) => Some(err),
            Error::Hyper(ref err) => Some(err),
            Error::Json(ref err) => Some(err),
            Error::Chrono(ref err) => Some(err),
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
struct ErrorResponse {
    ok: bool,
    error: String,
}

fn parse_error(body: &str) -> Result<String> {
    let er: ErrorResponse = try!(serde_json::from_str(body));
    Ok(er.error)
}

fn parse_response<T>(mut res: Response) -> Result<T> where T: Deserialize {
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

    fn request(&self, method: Method, url: &str) -> RequestBuilder {
        let auth = XStarfighterAuthorization(self.token.clone());
        self.client.request(method, &self.url(url)).header(auth)
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.request(Method::Get, "heartbeat").send());
        let hb: HeartbeatResponse = try!(parse_response(res));
        match (hb.ok, hb.error) {
            (false, Some(error)) => Err(Error::Unknown(error)),
            (false, None) => Err(Error::Unknown("<unknown>".to_owned())),
            (true, _) => Ok(()),
        }
    }

    pub fn account(&self, name: &str) -> Result<Account> {
        let name = name.to_owned();
        let account = Account { api: self, name: name };
        Ok(account)
    }
}

#[derive(Debug, Clone)]
pub struct Account<'a> {
    api: &'a Api,
    pub name: String,
}

impl<'a> Account<'a> {
    pub fn request(&self, method: Method, url: &str) -> RequestBuilder {
        self.api.request(method, url)
    }

    pub fn venue(&self, name: &str) -> Result<Venue> {
        Venue::new(self, name)
    }
}

#[derive(Debug)]
pub struct Venue<'a> {
    account: &'a Account<'a>,
    pub name: String,
}

#[derive(Deserialize, Debug)]
struct VenueHeartbeatResponse {
    ok: bool,
    error: Option<String>,
    venue: String,
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
    symbols: Vec<VenueStockSymbol>,
}

impl<'a> Venue<'a> {
    fn new(account: &'a Account, name: &str) -> Result<Self> {
        let venue = Venue { account: account, name: name.to_owned() };
        try!(venue.heartbeat());
        Ok(venue)
    }

    fn url(&self, url: &str) -> String {
        format!("venues/{}/{}", self.name, url)
    }

    fn request(&self, method: Method, url: &str) -> RequestBuilder {
        self.account.request(method, &self.url(url))
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.request(Method::Get, "heartbeat").send());
        let hb: VenueHeartbeatResponse = try!(parse_response(res));
        match (hb.ok, hb.error) {
            (false, Some(error)) => Err(Error::Unknown(error)),
            (false, None) => Err(Error::Unknown("<unknown>".to_owned())),
            (true, _) => Ok(()),
        }
    }

    pub fn stocks(&self) -> Result<Vec<Stock>> {
        let res = try!(self.request(Method::Get, "stocks").send());
        let vs: VenueStocksResponse = try!(parse_response(res));
        let stocks = vs.symbols.into_iter().map(|stock| {
            Stock { venue: self, symbol: stock.symbol, name: stock.name }
        }).collect::<Vec<_>>();
        Ok(stocks)
    }

    pub fn stock(&self, symbol: &str) -> Result<Stock> {
        let symbol = symbol.to_owned();
        let name = "<unknown>".to_owned();
        let stock = Stock { venue: self, symbol: symbol, name: name };
        Ok(stock)
    }
}

#[derive(Debug, Clone)]
pub struct Stock<'a> {
    pub venue: &'a Venue<'a>,
    pub symbol: String,
    pub name: String,
}

#[derive(Deserialize, Debug, Copy, Clone)]
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
    ts: String,
    venue: String,
    symbol: String,
    bids: Vec<StockOrder>,
    asks: Vec<StockOrder>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Direction {
    Buy,
    Sell,
}

impl Direction {
    pub fn as_str(&self) -> &'static str {
        match *self {
            Direction::Buy => "buy",
            Direction::Sell => "sell",
        }
    }
}

impl FromStr for Direction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Direction> {
        match s {
            "buy" => Ok(Direction::Buy),
            "sell" => Ok(Direction::Sell),
            _ => Err(Error::Unknown(format!("{}: invalid `direction`", s))),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OrderType {
    Limit,
    Market,
    FillOrKill,
    ImmediateOrCancel,
}

impl OrderType {
    pub fn as_str(&self) -> &'static str {
        match *self {
            OrderType::Limit => "limit",
            OrderType::Market => "market",
            OrderType::FillOrKill => "fill-or-kill",
            OrderType::ImmediateOrCancel => "immediate-or-cancel",
        }
    }
}

impl FromStr for OrderType {
    type Err = Error;

    fn from_str(s: &str) -> Result<OrderType> {
        match s {
            "limit" => Ok(OrderType::Limit),
            "market" => Ok(OrderType::Market),
            "fill-or-kill" => Ok(OrderType::FillOrKill),
            "immediate-or-cancel" => Ok(OrderType::ImmediateOrCancel),
            _ => Err(Error::Unknown(format!("{}: invalid `orderType`", s))),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct OrderRequest<'a> {
    pub account: &'a str,
    pub venue: &'a str,
    pub stock: &'a str,
    pub price: u64,
    pub qty: u64,
    pub direction: &'static str,
    #[serde(rename="orderType")]
    pub order_type: &'static str,
}

impl<'a> Stock<'a> {
    fn url(&self, url: Option<&str>) -> String {
        match url {
            None => format!("stocks/{}", self.symbol),
            Some(url) => format!("stocks/{}/{}", self.symbol, url),
        }
    }

    fn request(&self, method: Method, url: Option<&str>) -> RequestBuilder {
        self.venue.request(method, &self.url(url))
    }

    pub fn orders(&self) -> Result<StockOrders> {
        let res = try!(self.request(Method::Get, None).send());
        let so: StockOrdersResponse = try!(parse_response(res));
        let ts = try!(so.ts.parse::<DateTime<UTC>>());
        let orders = StockOrders { stock: self, ts: ts, bids: so.bids, asks: so.asks };
        Ok(orders)
    }

    pub fn order(&self, price: u64, qty: u64, direction: Direction, order_type: OrderType)
                 -> Result<OrderStatus> {
        let account = &self.venue.account.name;
        let venue = &self.venue.name;
        let stock = &self.symbol;
        let direction = direction.as_str();
        let order_type = order_type.as_str();
        let req = OrderRequest {
            account: account, venue: venue, stock: stock, price: price, qty: qty,
            direction: direction, order_type: order_type,
        };
        let req = try!(serde_json::to_string(&req));
        let res = try!(self.request(Method::Post, Some("orders")).body(&*req).send());
        let os: OrderStatusResponse = try!(parse_response(res));
        let status = try!(OrderStatus::new(self, os));
        Ok(status)
    }
}

#[derive(Debug, Clone)]
pub struct StockOrders<'a> {
    pub stock: &'a Stock<'a>,
    pub ts: DateTime<UTC>,
    pub bids: Vec<StockOrder>,
    pub asks: Vec<StockOrder>,
}

#[derive(Debug, Deserialize)]
struct FillResponse {
    price: u64,
    qty: u64,
    ts: String,
}

#[derive(Debug, Deserialize)]
struct OrderStatusResponse {
    ok: bool,
    symbol: String,
    venue: String,
    direction: String,
    #[serde(rename="originalQty")]
    original_qty: u64,
    qty: u64,
    price: u64,
    #[serde(rename="orderType")]
    order_type: String,
    id: u64,
    account: String,
    ts: String,
    fills: Vec<FillResponse>,
    #[serde(rename="totalFilled")]
    total_filled: u64,
    open: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Fill {
    pub price: u64,
    pub qty: u64,
    pub ts: DateTime<UTC>,
}

#[derive(Debug, Clone)]
pub struct OrderStatus<'a> {
    pub stock: &'a Stock<'a>,
    pub direction: Direction,
    pub original_qty: u64,
    pub qty: u64,
    pub price: u64,
    pub order_type: OrderType,
    pub id: u64,
    pub ts: DateTime<UTC>,
    pub fills: Vec<Fill>,
    pub total_filled: u64,
    pub open: bool,
}

impl<'a> OrderStatus<'a> {
    fn new(stock: &'a Stock, os: OrderStatusResponse) -> Result<Self> {
        assert_eq!(stock.symbol, os.symbol);
        assert_eq!(stock.venue.name, os.venue);
        assert_eq!(stock.venue.account.name, os.account);
        let mut fills = Vec::with_capacity(os.fills.len());
        for f in os.fills.into_iter() {
            let ts = try!(f.ts.parse::<DateTime<UTC>>());
            let fill = Fill { price: f.price, qty: f.qty, ts: ts };
            fills.push(fill);
        }
        let status = OrderStatus {
            stock: stock,
            direction: try!(os.direction.parse::<Direction>()),
            original_qty: os.original_qty,
            qty: os.qty,
            price: os.price,
            order_type: try!(os.order_type.parse::<OrderType>()),
            id: os.id,
            ts: try!(os.ts.parse::<DateTime<UTC>>()),
            fills: fills,
            total_filled: os.total_filled,
            open: os.open,
        };
        Ok(status)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static TOKEN: &'static str = "1193b20a8167398943e7bf22572e480fef59da47";

    #[test]
    fn test_heartbeat() {
        let client = Api::new(TOKEN);
        assert!(client.heartbeat().is_ok());
    }

    #[test]
    fn test_venue_heartbeat() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        assert!(venue.heartbeat().is_ok());
    }

    #[test]
    fn test_venue_stocks() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stocks = venue.stocks().unwrap();
        assert_eq!(1, stocks.len());
        assert_eq!("FOOBAR", stocks[0].symbol);
    }

    #[test]
    fn test_stock_orders() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let orders = stock.orders();
        assert!(orders.is_ok());
    }

    #[test]
    fn test_stock_new_order() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let status = stock.order(100, 10, Direction::Buy, OrderType::Limit).unwrap();
        assert_eq!(10, status.original_qty);
    }
}
