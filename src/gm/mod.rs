mod response;

use std::fmt;
use std::sync::Arc;

use hyper::client::{Client, RequestBuilder};
use hyper::header::{Headers, UserAgent, Accept, qitem};
use hyper::method::Method;
use hyper::mime::{Mime, TopLevel, SubLevel, Attr, Value};

use common::{XStarfighterAuthorization, parse_response, Result, Error};
pub use gm::response::{Status, StatusDetails, StatusFlash, StatusState};

static DEFAULT_API_URL: &'static str = "www.stockfighter.io/gm";

#[derive(Clone)]
pub struct Api {
    url: String,
    token: String,
    client: Arc<Client>,
}

impl fmt::Debug for Api {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Api {{ url: {:?}, token: {:?} }}", self.url, self.token)
    }
}

impl Api {
    pub fn new(token: &str) -> Self {
        Api {
            url: DEFAULT_API_URL.to_owned(),
            token: token.to_owned(),
            client: Arc::new(Client::new()),
        }
    }

    fn url(&self, url: &str) -> String {
        format!("https://{}/{}", self.url, url)
    }

    fn request(&self, method: Method, url: &str) -> RequestBuilder {
        let mut headers = Headers::new();
        headers.set(UserAgent(concat!("rogan", env!("CARGO_PKG_VERSION")).to_owned()));
        headers.set(Accept(vec![qitem(Mime(TopLevel::Application, SubLevel::Json,
                                           vec![(Attr::Charset, Value::Utf8)]))]));
        headers.set(XStarfighterAuthorization(self.token.clone()));
        self.client.request(method, &self.url(url)).headers(headers)
    }

    pub fn start(&self, level: &str) -> Result<Instance> {
        let res = try!(self.request(Method::Post, &format!("levels/{}", level)).send());
        let res: response::Instance = try!(parse_response(res));
        let mut inst = Instance::new(self.clone(), res);
        let status = try!(inst.status());
        if status.trading_day() > 0 { try!(inst.restart()); }
        Ok(inst)
    }

    pub fn resume(&self, level: &str) -> Result<Instance> {
        let res = try!(self.request(Method::Post, &format!("levels/{}", level)).send());
        let res: response::Instance = try!(parse_response(res));
        let mut inst = Instance::new(self.clone(), res);
        try!(inst.resume());
        Ok(inst)
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    api: Api,
    pub id: u64,
    pub account: String,
    pub symbols: Vec<String>,
    pub venues: Vec<String>,
    pub seconds_per_trading_day: u64,
}

impl Instance {
    fn new(api: Api, res: response::Instance) -> Self {
        Instance {
            api: api, id: res.instance_id, account: res.account, symbols: res.tickers,
            venues: res.venues, seconds_per_trading_day: res.seconds_per_trading_day,
        }
    }

    fn url(&self, url: Option<&str>) -> String {
        match url {
            None => format!("instances/{}", self.id),
            Some(url) => format!("instances/{}/{}", self.id, url),
        }
    }

    fn request(&self, method: Method, url: Option<&str>) -> RequestBuilder {
        self.api.request(method, &self.url(url))
    }

    pub fn resume(&mut self) -> Result<()> {
        let res = try!(self.request(Method::Post, Some("resume")).send());
        let res: response::Instance = try!(parse_response(res));
        *self = Instance::new(self.api.clone(), res);
        Ok(())
    }

    pub fn restart(&mut self) -> Result<()> {
        let res = try!(self.request(Method::Post, Some("restart")).send());
        let res: response::Instance = try!(parse_response(res));
        *self = Instance::new(self.api.clone(), res);
        Ok(())
    }

    pub fn stop(&mut self) -> Result<()> {
        let res = try!(self.request(Method::Post, Some("stop")).send());
        let res: response::Stop = try!(parse_response(res));
        match (res.ok, res.error) {
            (false, Some(error)) => Err(Error::Unknown(error)),
            (false, None) => Err(Error::Unknown("<unknown>".to_owned())),
            (true, _) => Ok(()),
        }
    }

    pub fn status(&self) -> Result<Status> {
        let res = try!(self.request(Method::Get, None).send());
        Ok(try!(parse_response(res)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static TOKEN: &'static str = "<valid token necessary for tests to pass>";

    #[test]
    fn test_it_works() {
        let api = Api::new(TOKEN);
        let mut inst = api.start("first_steps").unwrap();
        let id = inst.id;
        inst.resume().unwrap();
        assert_eq!(id, inst.id);
        inst.restart().unwrap();
        assert_eq!(id, inst.id);
        let status = inst.status().unwrap();
        assert_eq!(id, status.id);
        assert_eq!(false, status.done);
        inst.stop().unwrap();
    }
}
