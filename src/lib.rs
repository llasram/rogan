#![feature(custom_derive, custom_attribute, plugin)]
#![plugin(serde_macros)]

extern crate chrono;
#[macro_use] extern crate hyper;
extern crate serde;
extern crate serde_json;
extern crate websocket;

pub mod stockfighter;
