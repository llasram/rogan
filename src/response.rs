#[derive(Deserialize, Debug)]
pub struct Error {
    pub ok: bool,
    pub error: String,
}

#[derive(Deserialize, Debug)]
pub struct Heartbeat {
    pub ok: bool,
    pub error: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct VenueHeartbeat {
    pub ok: bool,
    pub error: Option<String>,
    pub venue: String,
}

#[derive(Deserialize, Debug)]
pub struct VenueStockSymbol {
    pub symbol: String,
    pub name: String,
}

#[derive(Deserialize, Debug)]
pub struct VenueStocks {
    pub ok: bool,
    pub error: Option<String>,
    pub symbols: Vec<VenueStockSymbol>,
}

#[derive(Deserialize, Debug, Copy, Clone)]
pub struct StockOrderbook {
    pub price: u64,
    pub qty: u64,
    #[serde(rename="isBuy")]
    pub is_buy: bool,
}

#[derive(Deserialize, Debug)]
pub struct Orderbook {
    pub ok: bool,
    pub error: Option<String>,
    pub ts: String,
    pub venue: String,
    pub symbol: String,
    pub bids: Vec<StockOrderbook>,
    pub asks: Vec<StockOrderbook>,
}

#[derive(Debug, Deserialize)]
pub struct Fill {
    pub price: u64,
    pub qty: u64,
    pub ts: String,
}

#[derive(Debug, Deserialize)]
pub struct OrderStatus {
    pub ok: bool,
    pub error: Option<String>,
    pub symbol: String,
    pub venue: String,
    pub direction: String,
    #[serde(rename="originalQty")]
    pub original_qty: u64,
    pub qty: u64,
    pub price: u64,
    #[serde(rename="orderType")]
    pub order_type: String,
    pub id: u64,
    pub account: String,
    pub ts: String,
    pub fills: Vec<Fill>,
    #[serde(rename="totalFilled")]
    pub total_filled: u64,
    pub open: bool,
}

#[derive(Debug, Deserialize)]
pub struct Quote {
    pub ok: bool,
    pub error: Option<String>,
    pub symbol: String,
    pub venue: String,
    pub bid: Option<u64>,
    pub ask: Option<u64>,
    #[serde(rename="bidSize")]
    pub bid_size: u64,
    #[serde(rename="askSize")]
    pub ask_size: u64,
    #[serde(rename="bidDepth")]
    pub bid_depth: u64,
    #[serde(rename="askDepth")]
    pub ask_depth: u64,
    pub last: Option<u64>,
    #[serde(rename="lastSize")]
    pub last_size: Option<u64>,
    #[serde(rename="lastTrade")]
    pub last_trade: Option<String>,
    #[serde(rename="quoteTime")]
    pub quote_time: String,
}

#[derive(Debug, Deserialize)]
pub struct OrderStatuses {
    pub ok: bool,
    pub error: Option<String>,
    pub venue: String,
    pub orders: Vec<OrderStatus>,
}
