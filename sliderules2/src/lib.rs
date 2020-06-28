use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

// Scale struct / class
#[derive(Serialize, Deserialize)]
pub struct Scale {
    name: String,
    y_ratio: f64,
    transformation: String,
    ranges: Vec<Range>,
}

// A range delimits a range over which a TickSpec operates
#[derive(Serialize, Deserialize)]
pub struct Range {
    start: f64,
    end: f64,
    spec: TickSpec,
}

// A tick spec defines how to generate ticks
#[derive(Serialize, Deserialize)]
pub struct TickSpec {
    count: u32, // How many sections in this range, count+1 ticks created
    template: Template, // What template to use on each tick
    incl: [bool; 2], // Whether to include starting/ending tick
    #[serde(default)]
    sub: HashMap<u16, TickSpec>, // Specs to run on subticks
}

/*
// Newtype for a HashMap that defaults to index 0 when only a single value is passed.
struct AutoNestHashMap<I, V>(HashMap<I, V>);

impl<I: Hash + Eq, V> Default for AutoNestHashMap<I, V> {
    fn default () -> Self {
        AutoNestHashMap(Default::default())
    }
}

impl<I: Hash + Eq + Serialize, V: Serialize> Serialize for AutoNestHashMap<I, V> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            AutoNestHashMap(hashmap) => {
                Serialize::serialize(hashmap, serializer)
            }
        }
    }
}

impl<'de, I: Hash + Eq + Deserialize<'de> + Default, V: Deserialize<'de>> Deserialize<'de> for AutoNestHashMap<I, V> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        let v_match : Result<V, D::Error> = Deserialize::deserialize::<D>(deserializer);
        let map : HashMap<I, V> = match v_match {
            Ok(v) => {
                let hm = HashMap::new();
                hm.insert(Default::default(), v);
                hm
            },
            Err(_) => {
                let hm : HashMap<I, V> = Deserialize::deserialize::<D>(deserializer)?;
                hm
            }
        };

        Ok(AutoNestHashMap(map))
    }
}
*/

// A template defines metadata about generating a tick given its position
#[derive(Serialize, Deserialize)]
pub struct Template {
    height: f64, // How tall the tick should be
    #[serde(default)]
    format: Option<Format>, // How to show the tick, if at all
}

// A format expresses how to convert a number to a string, using an initial "preshow"
// transformation on the number, and then a "show" transformation to turn it into a string
#[derive(Default, Serialize, Deserialize)]
pub struct Format {
    preshow: Preshow,
    show: Show,
}

#[derive(Serialize, Deserialize)]
pub enum Preshow {
    Id,             // Make no pretransformation
    LogClamp,       // Clamp it to the range [1,10) with successive division/multiplication
    LogNClamp(f64), // Clamp it to the range [1,n) with successive division/multiplication
}

impl Default for Preshow { fn default() -> Self { Preshow::Id } }

#[derive(Serialize, Deserialize)]
pub enum Show {
    Display,          // Display the float using Display (`format!("{}")`)
    Int,              // Display the float as an int, with rounding
    Float(u32, bool), // Display the float as a float with <n> digits precision
                      // The bool indicates whether to exclude everything before the digit
    Only(String),     // Only output this string - useful for things like pi
}

impl Default for Show { fn default() -> Self { Show::Display } }

// Main + External function bindings
#[wasm_bindgen]
pub fn main() {
    log("V2 WASM started.");
}

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
