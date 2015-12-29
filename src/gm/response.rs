use std::collections::BTreeMap;
use std::str::FromStr;

use common::{Error, Result};

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

#[derive(Debug, Deserialize, Clone)]
pub struct StatusDetails {
    #[serde(rename="endOfTheWorldDay")]
    pub end_of_the_world_day: u64,
    #[serde(rename="tradingDay")]
    pub trading_day: u64,
    #[serde(rename="executionTime")]
    pub execution_time: Option<f64>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct StatusFlash {
    pub info: Option<String>,
    pub warning: Option<String>,
    pub error: Option<String>,
    pub success: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatusState {
    Open,
    Won,
    Lost,
}

impl FromStr for StatusState {
    type Err = Error;

    fn from_str(s: &str) -> Result<StatusState> {
        match s {
            "open" => Ok(StatusState::Open),
            "won" => Ok(StatusState::Won),
            "lost" => Ok(StatusState::Lost),
            _ => Err(Error::Unknown(format!("{}: invalid `state`", s))),
        }
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct Status {
    pub ok: bool,
    pub error: Option<String>,
    pub done: bool,
    pub id: u64,
    pub state: String,
    pub flash: Option<StatusFlash>,
    pub details: Option<StatusDetails>,
}

impl Status {
    pub fn trading_day(&self) -> u64 {
        self.details.as_ref().map_or(0, |details| details.trading_day)
    }

    pub fn state(&self) -> Result<StatusState> {
        self.state.parse::<StatusState>()
    }
}

#[derive(Debug, Deserialize)]
pub struct Stop {
    pub ok: bool,
    pub error: Option<String>,
}
