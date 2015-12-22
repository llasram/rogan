use std::collections::BTreeMap;

#[derive(Debug, Deserialize)]
pub struct Instance {
    pub ok: bool,
    pub error: Option<String>,
    #[serde(rename="instanceId")]
    pub instance_id: u64,
    pub account: String,
    pub instructions: BTreeMap<String, String>,
    pub tickers: Vec<String>,
    pub venues: Vec<String>,
    #[serde(rename="secondsPerTradingDay")]
    pub seconds_per_trading_day: u64,
    pub balances: Option<BTreeMap<String, u64>>,
}

#[derive(Debug, Deserialize)]
pub struct StatusDetails {
    #[serde(rename="endOfTheWorldDay")]
    pub end_of_the_world_day: u64,
    #[serde(rename="tradingDay")]
    pub trading_day: u64,
    #[serde(rename="executionTime")]
    pub execution_time: Option<f64>,
}

#[derive(Debug, Deserialize)]
pub struct StatusFlash {
    pub info: Option<String>,
    pub error: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct Status {
    pub ok: bool,
    pub error: Option<String>,
    pub done: bool,
    pub id: u64,
    pub state: String,
    pub flash: Option<StatusFlash>,
    pub details: Option<StatusDetails>,
}

#[derive(Debug, Deserialize)]
pub struct Stop {
    pub ok: bool,
    pub error: Option<String>,
}
