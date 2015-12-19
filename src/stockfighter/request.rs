#[derive(Debug, Serialize)]
pub struct Order<'a> {
    pub account: &'a str,
    pub venue: &'a str,
    pub stock: &'a str,
    pub price: u64,
    pub qty: u64,
    pub direction: &'static str,
    #[serde(rename="orderType")]
    pub order_type: &'static str,
}
