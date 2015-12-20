mod request;
mod response;

use std::borrow::Borrow;
use std::convert::From;
use std::error;
use std::fmt;
use std::io;
use std::io::Read;
use std::result;
use std::str::FromStr;

use chrono;
use chrono::{DateTime, UTC};
use hyper;
use hyper::client::{Client, RequestBuilder, Response};
use hyper::method::Method;
use hyper::status::StatusCode;
use serde::Deserialize;
use serde_json;
use websocket;
use websocket::{Message, Sender, Receiver};
use websocket::message::Type as MessageType;

pub use stockfighter::response::StockOrderbook;

header! {
    (XStarfighterAuthorization, "X-Starfighter-Authorization") => [String]
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

static DEFAULT_API_URL: &'static str = "api.stockfighter.io/ob/api";

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

fn parse_error(body: &str) -> Result<String> {
    let er: response::Error = try!(serde_json::from_str(body));
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

impl Api {
    pub fn new(token: &str) -> Self {
        Api {
            url: DEFAULT_API_URL.to_owned(),
            token: token.to_owned(),
            client: hyper::Client::new()
        }
    }

    fn url(&self, url: &str) -> String {
        format!("https://{}/{}", self.url, url)
    }

    fn request(&self, method: Method, url: &str) -> RequestBuilder {
        let auth = XStarfighterAuthorization(self.token.clone());
        self.client.request(method, &self.url(url)).header(auth)
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.request(Method::Get, "heartbeat").send());
        let hb: response::Heartbeat = try!(parse_response(res));
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

impl<'a> Venue<'a> {
    fn new(account: &'a Account, name: &str) -> Result<Self> {
        let venue = Venue { account: account, name: name.to_owned() };
        try!(venue.heartbeat());
        Ok(venue)
    }

    fn url(&self, with_account: bool, url: &str) -> String {
        let venue = &self.name;
        let account = &self.account.name;
        match with_account {
            false => format!("venues/{}/{}", venue, url),
            true => format!("venues/{}/accounts/{}/{}", venue, account, url),
        }
    }

    fn ws_url(&self, url: &str) -> String {
        let base_url = &self.account.api.url;
        let account = &self.account.name;
        let venue = &self.name;
        format!("wss://{}/ws/{}/venues/{}/{}", base_url, account, venue, url)
    }

    fn request(&self, method: Method, with_account: bool, url: &str) -> RequestBuilder {
        self.account.request(method, &self.url(with_account, url))
    }

    pub fn heartbeat(&self) -> Result<()> {
        let res = try!(self.request(Method::Get, false, "heartbeat").send());
        let hb: response::VenueHeartbeat = try!(parse_response(res));
        match (hb.ok, hb.error) {
            (false, Some(error)) => Err(Error::Unknown(error)),
            (false, None) => Err(Error::Unknown("<unknown>".to_owned())),
            (true, _) => Ok(()),
        }
    }

    pub fn stocks(&self) -> Result<Vec<Stock>> {
        let res = try!(self.request(Method::Get, false, "stocks").send());
        let vs: response::VenueStocks = try!(parse_response(res));
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

    pub fn orders(&self) -> Result<Vec<Order>> {
        let res = try!(self.request(Method::Get, true, "orders").send());
        let res: response::OrderStatuses = try!(parse_response(res));
        let mut orders = Vec::with_capacity(res.orders.len());
        for status in res.orders.into_iter() {
            orders.push(try!(Order::new(self, status)));
        }
        Ok(orders)
    }

    pub fn ticker_tape(&self) -> Result<QuotesIter> {
        QuotesIter::new(self, &self.ws_url("tickertape"))
    }

    pub fn executions(&self) -> Result<ExecutionsIter> {
        ExecutionsIter::new(self, &self.ws_url("executions"))
    }
}

#[derive(Debug, Clone)]
pub struct Stock<'a> {
    pub venue: &'a Venue<'a>,
    pub symbol: String,
    pub name: String,
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

impl<'a> Stock<'a> {
    fn url(&self, in_account: bool, url: Option<&str>) -> String {
        let account = &self.venue.account.name;
        let symbol = &self.symbol;
        match (in_account, url) {
            (false, None) => format!("stocks/{}", self.symbol),
            (false, Some(url)) => format!("stocks/{}/{}", symbol, url),
            (true, None) => format!("accounts/{}/stocks/{}", account, symbol),
            (true, Some(url)) => format!("accounts/{}/stocks/{}/{}", account, symbol, url),
        }
    }

    fn ws_url(&self, url: &str) -> String {
        format!("{}/stocks/{}", self.venue.ws_url(url), self.symbol)
    }

    fn request(&self, method: Method, in_account: bool, url: Option<&str>) -> RequestBuilder {
        self.venue.request(method, false, &self.url(in_account, url))
    }

    pub fn orderbook(&self) -> Result<Orderbook> {
        let res = try!(self.request(Method::Get, false, None).send());
        let so: response::Orderbook = try!(parse_response(res));
        let ts = try!(so.ts.parse::<DateTime<UTC>>());
        let orders = Orderbook { stock: self, ts: ts, bids: so.bids, asks: so.asks };
        Ok(orders)
    }

    pub fn order(&self, price: u64, qty: u64, direction: Direction, order_type: OrderType)
                 -> Result<Order> {
        let req = request::Order {
            account: &self.venue.account.name,
            venue: &self.venue.name,
            stock: &self.symbol,
            price: price,
            qty: qty,
            direction: direction.as_str(),
            order_type: order_type.as_str(),
        };
        let req = try!(serde_json::to_string(&req));
        let res = try!(self.request(Method::Post, false, Some("orders")).body(&*req).send());
        let res: response::OrderStatus = try!(parse_response(res));
        let order = try!(Order::new(self.venue, res));
        Ok(order)
    }

    pub fn quote(&self) -> Result<Quote> {
        let res = try!(self.request(Method::Get, false, Some("quote")).send());
        let res: response::Quote = try!(parse_response(res));
        let quote = try!(Quote::new(self.venue, res));
        Ok(quote)
    }

    pub fn orders(&self) -> Result<Vec<Order>> {
        let res = try!(self.request(Method::Get, true, Some("orders")).send());
        let res: response::OrderStatuses = try!(parse_response(res));
        let mut orders = Vec::with_capacity(res.orders.len());
        for status in res.orders.into_iter() {
            orders.push(try!(Order::new(self.venue, status)));
        }
        Ok(orders)
    }

    pub fn ticker_tape(&self) -> Result<QuotesIter> {
        QuotesIter::new(self.venue, &self.ws_url("tickertape"))
    }

    pub fn executions(&self) -> Result<ExecutionsIter> {
        ExecutionsIter::new(self.venue, &self.ws_url("executions"))
    }
}

#[derive(Debug, Clone)]
pub struct Orderbook<'a> {
    pub stock: &'a Stock<'a>,
    pub ts: DateTime<UTC>,
    pub bids: Vec<StockOrderbook>,
    pub asks: Vec<StockOrderbook>,
}

#[derive(Debug, Clone, Copy)]
pub struct Fill {
    pub price: u64,
    pub qty: u64,
    pub ts: DateTime<UTC>,
}

#[derive(Debug, Clone)]
pub struct Order<'a> {
    pub venue: &'a Venue<'a>,
    pub symbol: String,
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

impl<'a> Order<'a> {
    fn new(venue: &'a Venue, os: response::OrderStatus) -> Result<Self> {
        assert_eq!(venue.name, os.venue);
        assert_eq!(venue.account.name, os.account);
        let mut fills = Vec::with_capacity(os.fills.len());
        for f in os.fills.into_iter() {
            let ts = try!(f.ts.parse::<DateTime<UTC>>());
            let fill = Fill { price: f.price, qty: f.qty, ts: ts };
            fills.push(fill);
        }
        let order = Order {
            venue: venue,
            symbol: os.symbol.clone(),
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
        Ok(order)
    }

    fn url(&self, url: Option<&str>) -> String {
        match url {
            None => format!("stocks/{}/orders/{}", self.symbol, self.id),
            Some(url) => format!("stocks/{}/orders/{}/{}", self.symbol, self.id, url),
        }
    }

    fn request(&self, method: Method, url: Option<&str>) -> RequestBuilder {
        self.venue.request(method, false, &self.url(url))
    }

    pub fn update(&mut self) -> Result<()> {
        let res = try!(self.request(Method::Get, None).send());
        let os: response::OrderStatus = try!(parse_response(res));
        *self = try!(Order::new(self.venue, os));
        Ok(())
    }

    pub fn cancel(&mut self) -> Result<()> {
        let res = try!(self.request(Method::Delete, None).send());
        let os: response::OrderStatus = try!(parse_response(res));
        *self = try!(Order::new(self.venue, os));
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct QuoteState {
    pub price: Option<u64>,
    pub size: u64,
    pub depth: u64,
}

#[derive(Debug, Clone)]
pub struct Quote<'a> {
    pub venue: &'a Venue<'a>,
    pub symbol: String,
    pub ts: DateTime<UTC>,
    pub bid: QuoteState,
    pub ask: QuoteState,
    pub last: Option<Fill>,
}

impl<'a> Quote<'a> {
    fn new(venue: &'a Venue, res: response::Quote) -> Result<Self> {
        assert_eq!(venue.name, res.venue);
        let ts = try!(res.quote_time.parse::<DateTime<UTC>>());
        let last = match (res.last, res.last_size, res.last_trade) {
            (Some(price), Some(qty), Some(ts)) => {
                let ts = try!(ts.parse::<DateTime<UTC>>());
                let fill = Fill { price: price, qty: qty, ts: ts };
                Some(fill)
            }
            (None, None, None) => None,
            _ => return Err(Error::Unknown("inconsistent last-trade state".to_owned())),
        };
        let quote = Quote {
            venue: venue, symbol: res.symbol, ts: ts, last: last,
            bid: QuoteState { price: res.bid, size: res.bid_size, depth: res.bid_depth },
            ask: QuoteState { price: res.ask, size: res.ask_size, depth: res.ask_depth },
        };
        Ok(quote)
    }

    fn url(&self) -> String {
        format!("stocks/{}/quote", self.symbol)
    }

    fn request(&self, method: Method) -> RequestBuilder {
        self.venue.request(method, false, &self.url())
    }

    pub fn stock(&self) -> Result<Stock> {
        self.venue.stock(&self.symbol)
    }

    pub fn update(&mut self) -> Result<()> {
        let res = try!(self.request(Method::Get).send());
        let res: response::Quote = try!(parse_response(res));
        *self = try!(Quote::new(self.venue, res));
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OrderPosition {
    Standing,
    Incoming,
}

impl OrderPosition {
    pub fn as_str(&self) -> &'static str {
        match *self {
            OrderPosition::Standing => "standing",
            OrderPosition::Incoming => "incoming",
        }
    }
}

impl FromStr for OrderPosition {
    type Err = Error;

    fn from_str(s: &str) -> Result<OrderPosition> {
        match s {
            "standing" => Ok(OrderPosition::Standing),
            "incoming" => Ok(OrderPosition::Incoming),
            _ => Err(Error::Unknown(format!("{}: invalid `OrderPosition`", s))),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OrderState {
    pub id: u64,
    pub open: bool,
}

#[derive(Debug, Clone)]
pub struct Execution<'a> {
    pub order: Order<'a>,
    pub fill: Fill,
    pub direction: OrderPosition,
    pub matched: OrderState,
}

impl<'a> Execution<'a> {
    fn new(venue: &'a Venue, res: response::Execution) -> Result<Self> {
        let ts = try!(res.filled_at.parse::<DateTime<UTC>>());
        let fill = Fill { price: res.price, qty: res.filled, ts: ts };
        let order = try!(Order::new(venue, res.order));
        let direction = match order.id == res.standing_id {
            true => OrderPosition::Standing,
            false => OrderPosition::Incoming,
        };
        let matched_id = match direction {
            OrderPosition::Standing => res.incoming_id,
            OrderPosition::Incoming => res.standing_id,
        };
        let matched_open = !match direction {
            OrderPosition::Standing => res.incoming_complete,
            OrderPosition::Incoming => res.standing_complete,
        };
        let matched = OrderState { id: matched_id, open: matched_open };
        let exec = Execution {
            order: order, fill: fill, direction: direction, matched: matched,
        };
        Ok(exec)
    }
}

type WebSocketReceiver = websocket::client::receiver::Receiver<websocket::stream::WebSocketStream>;
type WebSocketSender = websocket::client::sender::Sender<websocket::stream::WebSocketStream>;

pub struct QuotesIter<'a> {
    venue: &'a Venue<'a>,
    receiver: WebSocketReceiver,
    sender: WebSocketSender,
}

impl<'a> QuotesIter<'a> {
    fn new(venue: &'a Venue, url: &str) -> Result<QuotesIter<'a>> {
        let url =  websocket::client::request::Url::parse(url).unwrap();
        let req = try!(websocket::Client::connect(url));
        let res = try!(req.send());
        try!(res.validate());
        let client = res.begin();
        let (sender, receiver) = client.split();
        let iter = QuotesIter { venue: venue, receiver: receiver, sender: sender };
        Ok(iter)
    }

    fn recv_quote(&mut self) -> Result<Option<Quote<'a>>> {
        loop {
            let msg: Message = try!(self.receiver.recv_message());
            match msg.opcode {
                MessageType::Close => {
                    try!(self.sender.send_message(&Message::close()));
                    return Ok(None);
                },
                MessageType::Ping => {
                    try!(self.sender.send_message(&Message::pong(msg.payload)));
                },
                MessageType::Text => {
                    let payload: &[u8] = msg.payload.borrow();
                    let res: response::TickerTape = try!(serde_json::from_slice(payload));
                    return match (res.ok, res.error, res.quote) {
                        (false, Some(error), _) => Err(Error::Unknown(error)),
                        (true, _, Some(res)) => Ok(Some(try!(Quote::new(self.venue, res)))),
                        _ => Err(Error::Unknown("<unknown>".to_owned())),
                    }
                },
                _ => (),
            }
        }
    }
}

impl<'a> Iterator for QuotesIter<'a> {
    type Item = Result<Quote<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.recv_quote() {
            Ok(None) => None,
            Ok(Some(quote)) => Some(Ok(quote)),
            Err(err) => Some(Err(err)),
        }
    }
}

pub struct ExecutionsIter<'a> {
    venue: &'a Venue<'a>,
    receiver: WebSocketReceiver,
    sender: WebSocketSender,
}

impl<'a> ExecutionsIter<'a> {
    fn new(venue: &'a Venue, url: &str) -> Result<ExecutionsIter<'a>> {
        let url =  websocket::client::request::Url::parse(url).unwrap();
        let req = try!(websocket::Client::connect(url));
        let res = try!(req.send());
        try!(res.validate());
        let client = res.begin();
        let (sender, receiver) = client.split();
        let iter = ExecutionsIter { venue: venue, receiver: receiver, sender: sender };
        Ok(iter)
    }

    fn recv_execution(&mut self) -> Result<Option<Execution<'a>>> {
        loop {
            let msg: Message = try!(self.receiver.recv_message());
            match msg.opcode {
                MessageType::Close => {
                    try!(self.sender.send_message(&Message::close()));
                    return Ok(None);
                },
                MessageType::Ping => {
                    try!(self.sender.send_message(&Message::pong(msg.payload)));
                },
                MessageType::Text => {
                    let payload: &[u8] = msg.payload.borrow();
                    let res: response::Execution = try!(serde_json::from_slice(payload));
                    return Ok(Some(try!(Execution::new(self.venue, res))));
                },
                _ => (),
            }
        }
    }
}

impl<'a> Iterator for ExecutionsIter<'a> {
    type Item = Result<Execution<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.recv_execution() {
            Ok(None) => None,
            Ok(Some(exec)) => Some(Ok(exec)),
            Err(err) => Some(Err(err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static TOKEN: &'static str = "<valid token necessary for tests to pass>";

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
    fn test_stock_orderbook() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let book = stock.orderbook();
        assert!(book.is_ok());
    }

    #[test]
    fn test_stock_new_order() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let order = stock.order(100, 10, Direction::Buy, OrderType::Limit).unwrap();
        assert_eq!(10, order.original_qty);
    }

    #[test]
    fn test_stock_quote() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let quote = stock.quote();
        assert!(quote.is_ok());
    }

    #[test]
    fn test_stock_quote_update() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let mut quote = stock.quote().unwrap();
        let ts = quote.ts;
        ::std::thread::sleep(::std::time::Duration::new(1, 0));
        quote.update().unwrap();
        assert!(ts < quote.ts);
    }

    #[test]
    fn test_stock_order_update() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let mut order = stock.order(100, 10, Direction::Buy, OrderType::Limit).unwrap();
        let ts1 = order.ts.clone();
        assert!(order.update().is_ok());
        assert!(ts1 <= order.ts);
    }

    #[test]
    fn test_stock_order_cancel() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let mut order = stock.order(100, 10, Direction::Buy, OrderType::Limit).unwrap();
        assert!(order.open);
        assert!(order.cancel().is_ok());
        assert!(!order.open);
    }

    #[test]
    fn test_venue_orders() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let order = stock.order(100, 10, Direction::Buy, OrderType::Limit);
        assert!(order.is_ok());
        let orders = venue.orders().unwrap();
        assert!(0 < orders.len());
    }

    #[test]
    fn test_stock_orders() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        ::std::thread::sleep(::std::time::Duration::new(1, 0));
        let order = stock.order(100, 10, Direction::Buy, OrderType::Limit);
        assert!(order.is_ok());
        let orders = stock.orders().unwrap();
        assert!(0 < orders.len());
        ::std::thread::sleep(::std::time::Duration::new(1, 0));
        let order = stock.order(100, 10, Direction::Sell, OrderType::Limit);
        assert!(order.is_ok());
        ::std::thread::sleep(::std::time::Duration::new(1, 0));
        let order = stock.order(100, 10, Direction::Sell, OrderType::Limit);
        assert!(order.is_ok());
    }

    #[test]
    fn test_venue_ticker_tape() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let mut ticker = venue.ticker_tape().unwrap();
        let quote = ticker.next().unwrap();
        assert!(quote.is_ok());
    }

    #[test]
    fn test_stock_ticker_tape() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let mut ticker = stock.ticker_tape().unwrap();
        let quote = ticker.next().unwrap();
        assert!(quote.is_ok());
    }

    #[test]
    fn test_venue_executions() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let mut execs = venue.executions().unwrap();
        let exec = execs.next().unwrap();
        assert!(exec.is_ok());
    }

    #[test]
    fn test_stock_executions() {
        let api = Api::new(TOKEN);
        let account = api.account("EXB123456").unwrap();
        let venue = account.venue("TESTEX").unwrap();
        let stock = venue.stock("FOOBAR").unwrap();
        let mut execs = stock.executions().unwrap();
        let exec = execs.next().unwrap();
        assert!(exec.is_ok());
    }
}
