use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

#[wasm_bindgen]
pub fn sum_js (val: JsValue) -> i16 {
    sum(val.into_serde().unwrap())
}

pub fn sum (map: HashMap<i16, i16>) -> i16 {
    let mut total = 0;
    for (key, val) in map {
        total += key;
        total += val;
    }
    return total;
}

#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Tick {
    value: f32,
    position: f32,
    height: f32,
    name: Option<String>,
}

impl Tick {
    fn render_height (&self, scale: f32) -> f32 { self.height * 1000.0 * scale }
    fn render_position (&self) -> f32 { self.position * 1000.0 }
    fn to_line (&self, scale: f32) -> String {
        format!(" M {} 0 l 0 {} ", self.render_position(), self.render_height(scale))
    }
    fn font_height (&self, scale: f32) -> f32 { self.render_height(scale) * 0.4 }
    fn full_height (&self, scale: f32) -> f32 { self.render_height(scale) + self.font_height(scale) }
    fn to_text (&self, scale: f32) -> Option<String> {
        Some(format!("<text text-anchor=\"middle\" x=\"{x}\" y=\"{y}\" font-size=\"{font_size}\">{content}</text>",
            x=self.render_position(),
            y=self.render_height(scale)+self.font_height(scale),
            font_size=self.font_height(scale),
            content=self.name.as_ref()?
        ))
    }
}

pub fn ticks_to_path (ticks: &Vec<Tick>, scale: f32) -> String {
    let mut result = String::from("<path d=\"");

    for tick in ticks {
        result.push_str(&tick.to_line(scale));
    }
    result.push_str("\" vector-effect=\"non-scaling-stroke\"></path>");

    return result;
}

pub fn ticks_to_texts (ticks: &Vec<Tick>, scale: f32) -> String {
    let mut result = String::from("");

    for tick in ticks {
        if let Some(str) = tick.to_text(scale) {
            result.push_str(&str);
        }
    }

    return result;
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Format {
    show: Show,
    preshow: PreShow,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum PreShow {
    Nothing,
    LogClamp(f32),
    #[serde(skip)]
    Custom(js_sys::Function)
}

impl Default for PreShow {
    fn default() -> Self { PreShow::Nothing }
}

impl PreShow {
    fn run (&self, mut val: f32) -> f32 {
        match &self {
            PreShow::Nothing => {},
            PreShow::LogClamp(r) => {
                let r = *r;

                if r == 1.0 {
                    return 1.0;
                } else {
                    while val < 1.0 {
                        val *= r;
                    }

                    while val >= r {
                        val /= r;
                    }
                }
            },
            PreShow::Custom(f) => {
                let jsval = JsValue::from(val);

                match f.call1(&JsValue::NULL, &jsval) {
                    Err(_) => {},
                    Ok(res) => {
                        match res.as_f64() {
                            None => {},
                            Some(res) => val = res as f32,
                        }
                    }
                };
            }
        }

        val
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Show {
    Nothing,
    Int,
    Float(usize),
    Only(String),
    #[serde(skip)]
    Custom(js_sys::Function)
}

impl Default for Show {
    fn default() -> Self { Show::Nothing }
}

impl Show {
    fn run (&self, val: f32) -> Option<String> {

        match self {
            Show::Nothing => None,
            Show::Int => Some(format!("{:.0}", val)),
            Show::Float(prec) => Some(format!("{:.*}", prec, val)),
            Show::Only(string) => Some(string.to_string()),
            Show::Custom(f) => {
                let val = JsValue::from(val);
                match f.call1(&JsValue::NULL, &val) {
                    Err(_) => None,
                    Ok(val) => {
                        match val.into_serde() {
                            Err(_) => None,
                            Ok(s) => Some(s)
                        }
                    }
                }
            },
        }
    }
}

impl Format {
    fn run (&self, val: f32) -> Option<String> {
        self.show.run(self.preshow.run(val))
    }
}

#[wasm_bindgen]
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Template {
    height: f32,
    #[serde(default)]
    show: Format,
}

#[wasm_bindgen]
pub fn template_js (val: &JsValue) -> Template {
    val.into_serde().unwrap()
}

#[wasm_bindgen]
pub fn template (height: f32, show: js_sys::Function) -> Template {
    Template {
        height: height,
        show: Format {
            show: Show::Custom(show),
            preshow: PreShow::Nothing,
        }
    }
}

#[wasm_bindgen]
pub fn template_noshow (height: f32) -> Template {
    Template {
        height: height,
        show: Format {
            show: Show::Nothing,
            preshow: PreShow::Nothing,
        }
    }
}

#[wasm_bindgen]
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Spec {
    count: u16,
    #[serde(default)]
    incl_start: bool,
    #[serde(default)]
    incl_end: bool,
    #[serde(default)]
    template: Template,
    #[serde(default)]
    sub_specs: HashMap<u16, Spec>,
}

#[wasm_bindgen]
pub fn spec_noshow (val: &JsValue) -> Spec {
    val.into_serde().unwrap()
}

#[wasm_bindgen]
pub fn spec (val: &JsValue, show: js_sys::Function) -> Spec {
    let mut v : Spec = val.into_serde().unwrap();
    v.template.show = Format {
        show: Show::Custom(show),
        preshow: PreShow::Nothing,
    };
    return v;
}

fn to_tick(value: f32, template: &Template, transform: &Transform) -> Tick {
    Tick {
        value: value,
        position: transform.run(value),
        height: template.height,
        name: template.show.run(value),
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Transform {
    Id,
    Inv,
    Linear(f32, f32),
    Pow,
    PowN(f32),
    Exp,
    ExpN(f32),
    Log,
    LogN(f32),
    LogLog,
    Sin,
    Cos,
    Tan,
    Compose(Vec<Transform>)
}

impl Transform {
    fn run(&self, x: f32) -> f32 {
        use Transform::*;

        match self {
            Id => x,
            Inv => 1.0 / x,
            Linear(a,b) => x * a + b,
            Pow => x.powf(10.0),
            PowN(n) => x.powf(*n),
            Exp => (10.0 as f32).powf(x),
            ExpN(n) => n.powf(x),
            Log => x.log(10.0),
            LogN(n) => x.log(*n),
            LogLog => x.log(10.0).log(10.0),
            Sin => x.sin(),
            Cos => x.cos(),
            Tan => x.tan(),
            Compose(transforms) => {
                let mut res = x;
                for transform in transforms {
                    res = transform.run(res);
                }
                res
            },
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Scale {
    name: String,
    scale: f32,
    transform: Transform,
    specs: Vec<StartEnd>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StartEnd {
    start: f32,
    end: f32,
    spec: Spec,
}

#[wasm_bindgen]
pub fn scale (val: &JsValue) -> JsValue {
    let scale : Scale = val.into_serde().unwrap();
    JsValue::from_serde(&scale).unwrap()
}

#[wasm_bindgen]
pub fn unfold_js(scale: JsValue) -> JsValue {
    JsValue::from_serde(&unfold(&scale.into_serde().unwrap())).unwrap()
}

pub fn unfold(scale: &Scale) -> Vec<Tick> {
    let mut vec = vec![];
    for start_end in &scale.specs {
        unfold_f(&mut vec, start_end.start, start_end.end, &scale.transform, &start_end.spec);
    }
    return vec;
}

pub fn unfold_f(vec: &mut Vec<Tick>, mut start: f32, end: f32, transform: &Transform, spec: &Spec) {
    let delta = (end - start) / spec.count as f32;
    let mut curr_sub_spec: Option<&Spec> = None;

    for x in 0..spec.count {
        if spec.incl_start || x != 0 {
            vec.push(to_tick(start, &spec.template, transform));
        }

        match spec.sub_specs.get(&x) {
            None => {},
            Some(sub_spec) => {
                curr_sub_spec = Some(sub_spec);
            }
        }

        match &curr_sub_spec {
            None => {},
            Some(sub_spec) => {
                unfold_f(vec, start, start+delta, transform, sub_spec);
            },
        }
        start += delta;
    }

    if spec.incl_end {
        vec.push(to_tick(start, &spec.template, transform));
    }
}

#[wasm_bindgen]
pub fn scale_to_svg_js (val: JsValue) -> String {
    scale_to_svg(&val.into_serde().unwrap())
}

pub fn scale_to_svg (scale: &Scale) -> String {
    let ticks = unfold(scale);

    //let mut min_x : f32 = 0.0;
    let mut min_y : f32 = 0.0;
    //let mut max_x : f32 = 0.0;
    let mut max_y : f32 = 0.0;

    let mut min_x : f32 = -100.0;
    let mut max_x : f32 = 1020.0;

    for tick in &ticks {
        // Update x bounds
        // min_x = min_x.min(tick.render_position());
        // max_x = max_x.max(tick.render_position());

        // Update y bounds
        min_y = min_y.min(tick.full_height(scale.scale));
        max_y = max_y.max(tick.full_height(scale.scale));
    }

    // let delta_x = (max_x - min_x) * 0.02;
    // min_x -= delta_x;
    // max_x += delta_x;
    let width = max_x-min_x;
    let height = max_y-min_y;

    format!("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{min_x} {min_y} {width} {height}\">
                <g id=\"svgGroup\" stroke-linecap=\"round\" fill-rule=\"evenodd\"
                   stroke=\"#000\" stroke-width=\"0.25mm\" fill=\"none\"
                   style=\"stroke:#000;stroke-width:0.25mm;fill:none\">
                   {line}
                </g>
                {texts}
                <text text-anchor=\"middle\" x=\"-50\" y=\"{name_offset}\" font-size=\"{name_height}\">
                    {name}
                </text>
            </svg>",
        min_x=min_x,
        min_y=min_y,
        width=width,
        height=height,
        line=ticks_to_path(&ticks, scale.scale),
        texts=ticks_to_texts(&ticks, scale.scale),
        name_offset=height*0.7,
        name_height=height*0.5,
        name=scale.name
    )
}

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
