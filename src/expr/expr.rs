use std::default::Default;
use std::hash::Hash;
use std::sync::Arc;
use std::collections::*;
use std::fmt::Display;
use num_notation::*;
use num_traits::Pow;
use std::hash::*; // hash for expr needs to be implemented
use crate::mono::mono::*;

struct symbol (HashMap<String, String>);

impl <'a> Default for symbol {
	fn default() -> Self {
		Self (HashMap::from([
			(String::from("PLUS"),		String::from("+")),
			(String::from("MINUS"),		String::from("-")),
			(String::from("TIMES"),		String::from("*")),
			(String::from("DIV"),		String::from("/")),
			(String::from("NEGATE"),	String::from("~")),
			(String::from("SQRT"),		String::from("Sqrt")),
			(String::from("SQUARE"),	String::from("Sqr")),
			(String::from("SIN"),		String::from("Sin")),
			(String::from("COS"),		String::from("Cos")),
			(String::from("ASIN"),		String::from("Asin")),
			(String::from("ACOS"),		String::from("Acos")),
			(String::from("POWER"),		String::from("^")),
		]))
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumExpr {
	PARAM		= 0,
	PARAM_PTR	= 1,
	POINT 		= 10,
	ENTITY 		= 11,
	CONSTANT 	= 20,
	PLUS 		= 100,
	MINUS 		= 101,
	TIMES 		= 102,
	DIV 		= 103,
    POW         = 104,
	NEGATE 		= 105,
	SQRT 		= 106,
	SQUARE 		= 107,
	SIN 		= 108,
	COS 		= 109,
	ASIN		= 110,
	ACOS 		= 111,

	ALL_RESOLVED= 1000,
	PAREN		= 1001,
	BINARY_OP 	= 1002,
	UNARY_OP	= 1003,
}

impl Display for EnumExpr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let enum_op:HashMap<usize, &str> = HashMap::from([
			(0,			"PARAM"),
			(1,			"PARAM_PTR"),
			(10,		"POINT"),
			(11,		"ENTITY"),
			(20,		"CONSTANT"),
			(100,		"PLUS"),
			(101,		"MINUS"),
			(102,		"TIMES"),
			(103,		"DIV"),
			(104,       "POW"),
			(105,		"NEGATE"),
			(106,       "SQRT"),
			(107,       "SQUARE"),
			(108,       "SIN"),
			(109,       "COS"),
			(110,       "ASIN"),
			(111,       "ACOS"),
			(1000,      "ALL_RESOLVED"),
			(1001,      "PAREN"),
			(1002,      "BINARY_OP"),
			(1003,      "UNARY_OP"),
			
		]);
		let op_string: &str = enum_op.get(&(self.clone() as usize)).unwrap();
		write!(f, "{}", op_string)
	}
}

#[derive(Debug)]
pub enum XExpr {
    v (f64),
    c (String),
}

pub trait Sqrt {
	fn sqrt(self) -> Self;
}

pub trait Powi {
	fn powi(self, exp: i32) -> Self;
}

// impl From<Number> for f64 {
// 	fn from(value: Number) -> Self {
// 		match value {
// 			Number::Decimal(val) => val,
// 			_ => panic!(not implemented )
// 		}
// 	}
// }

// impl Into<f64> for Number{
// 	fn into(self) -> f64 {
// 		match self {
// 			Number::Decimal(val) => val,
// 			_ => panic!("not implemented")
// 		}
// 	}
// }

impl Sqrt for Number {
	fn sqrt(self) -> Self {
		match self {
			Number::Decimal(val) => Number::Decimal(val.sqrt()),
			_ => panic!("other forms not implemented for now")
		}
	}
}

impl Powi for Number {
	fn powi(self, exp: i32) -> Self {
		match self {
			Number::Decimal(val) => Number::Decimal(val.powi(exp)),
			_ => panic!("other forms not implemented")
		}
	}
}

pub type Variables = BTreeMap<String, Number>;

#[derive(Debug, Clone)]
pub struct Term {
	coefficient: Number,
	variables: Variables
}

impl Term {
	pub fn coefficient(&self) -> &Number {
		return &self.coefficient;
	}

	pub fn variables(&self) -> &Variables {
		return &self.variables;
	}
}

impl Display for XExpr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			XExpr::v(value) => write!(f, "{}", value),
			XExpr::c(value) => write!(f, "{}", value),
		}
	}
}

impl Display for Term {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.variables.is_empty(){
			write!(f, "{}", self.coefficient)
		}
		else {
			write!(f, "{}", self.coefficient)?;
			for (s, n) in self.variables.iter() {
				write!(f, "*{}^{}", s, n)?;
			}
			Ok(())
		}
	}
}

impl Default for Term {
	fn default() -> Self {
		Self { coefficient: (Number::Decimal(0.0)), variables: (Default::default()) }
	}
}

#[derive(Clone, Debug)]
pub enum poly_Check{
	bool (bool),
	map (HashMap<String, Mono>),
}

#[derive(Clone, Debug)]
pub struct Expr {
    // PARAM: f64,
    a: Option<Arc<Expr>>,
    b: Option<Arc<Expr>>,
    op: EnumExpr,
    x: Term,
}

impl PartialEq for Expr {
	fn eq(&self, other: &Expr) -> bool{
		format!("{}", self) == format!("{}", other)
	}
}

impl Default for Expr {
    fn default() -> Self {
        Expr {
            a: None,
            b: None,
            op: EnumExpr::PARAM,
            x: Default::default(),
        }
    }
}

impl Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		// formatting based on operation and children
		let enum_op:HashMap<usize, &str> = HashMap::from([
			(0,			"PARAM"),
			(1,			"PARAM_PTR"),
			(10,		"POINT"),
			(11,		"ENTITY"),
			(20,		"CONSTANT"),
			(100,		"PLUS"),
			(101,		"MINUS"),
			(102,		"TIMES"),
			(103,		"DIV"),
			(104,       "POW"),
			(105,		"NEGATE"),
			(106,		"SQRT"),
			(107,		"SQUARE"),
			(108,		"SIN"),
			(109,		"COS"),
			(110,		"ASIN"),
			(111,		"ACOS"),
			(1000,		"ALL_RESOLVED"),
			(1001,		"PAREN"),
			(1002,		"BINARY_OP"),
			(1003,		"UNARY_OP"),
		]);
		let op_symbols:HashMap<String, String> = HashMap::from([
			(String::from("PLUS"),		String::from("+")),
			(String::from("MINUS"),		String::from("-")),
			(String::from("TIMES"),		String::from("*")),
			(String::from("DIV"),		String::from("/")),
			(String::from("NEGATE"),	String::from("~")),
			(String::from("SQRT"),		String::from("Sqrt")),
			(String::from("SQUARE"),	String::from("Sqr")),
			(String::from("SIN"),		String::from("Sin")),
			(String::from("COS"),		String::from("Cos")),
			(String::from("ASIN"),		String::from("Asin")),
			(String::from("ACOS"),		String::from("Acos")),
			(String::from("POWER"),		String::from("^")),

		]);
		if self.children() == 0 && self.op == EnumExpr::CONSTANT {
			return write!(f, "{}", self.x);
		}
		else if self.children() == 0 && self.op == EnumExpr::PARAM {
			return write!(f, "{}", self.x)
		}
		else {
			if self.children() == 2 {
				// Unary operation formatting
				let str_a: String = self.a.clone().unwrap().to_string();
				let str_b: String = self.b.clone().unwrap().to_string();
				let op_string: String = String::from(*enum_op.get(&(self.op.clone() as usize)).unwrap());
				let sym: &String = op_symbols.get(&op_string).unwrap();
				return write!(f, "{} {} {}", str_a, sym, str_b);
			}
			else if self.children() == 1 {
				// Binary operation formatting
				let str_a: String = self.a.clone().unwrap().to_string();
				let op_string: String = String::from(*enum_op.get(&(self.op.clone() as usize)).unwrap());
				let sym: &String = op_symbols.get(&op_string).unwrap();
				return write!(f, "{} {}", sym, str_a);
			}
			else{
				return write!(f, "Unknown Expression");
			}
		}
	}
}

impl Hash for Expr {
    // Note: No generic parameters for Self here
    fn hash<H: Hasher>(&self, state: &mut H) {
		let fmt_expr = format!("{}", self); 
		fmt_expr.hash(state);
    }
}

impl From<f64> for Expr {
	fn from(value: f64) -> Self {
		Expr {
			a: None,
			b: None,
			op: EnumExpr::CONSTANT,
			x: Term {
				coefficient: Number::Decimal(value),
				variables: Variables::new()
			}
		} 
	}
}

impl From<&str> for Expr {
	fn from(n: &str) -> Self{
			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: Number::Decimal(1.0),
				variables: Variables::from([(n.to_string(), Number::Decimal(1.0 ))]),
			}
		}
	}
}

impl From<String> for Expr {
	fn from(n: String) -> Self{
			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: Number::Decimal(1.0),
				variables: Variables::from([(n, Number::Decimal(1.0 ))]),
			}
		}
	}
}

impl From<Number> for Expr {
	fn from(n: Number) -> Self{
			Expr{
			a: None,
			b: None,
			op: EnumExpr::CONSTANT,
			x: Term {
				coefficient: n,
				..Default::default()
			}
		}
	}
}

impl From<(String, Number)> for Expr{
	fn from((s, n): (String, Number)) -> Self {
			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: Number::Decimal(1.0),
				variables: Variables::from([(s, n)])
			}
		}
	}
}

impl From<(Number, String, Number)> for Expr {
	fn from((c, s, n): (Number, String, Number)) -> Self{
			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: c,
				variables: Variables::from([(s, n)])
			}
		}
	}
}

impl From<(Number, Vec<String>, Vec<Number>)> for Expr {
	fn from((c, s, n): (Number, Vec<String>, Vec<Number>)) -> Self{
			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: c,
				variables: Variables::from(s.into_iter().zip(n.into_iter()).collect::<BTreeMap<String, Number>>())
			}
		}
	}
}

impl From<Term> for Expr {
	fn from(c: Term) -> Self{
			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: c
		}
	}
}

impl From<Mono> for Expr {
	fn from(m: Mono) -> Self{
			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: m.coefficient().clone(),
				variables: m.variables().clone(),
			}
		}
	}
}

impl From<(Vec<Number>, Vec<String>, Vec<Number>)> for Expr {
	fn from((c, s, n): (Vec<Number>, Vec<String>, Vec<Number>)) -> Self{
			let mut res: Number = Number::Decimal(1.0);
			for num in c.into_iter() {
				res *= num;
			}

			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: res,
				variables: Variables::from(s.into_iter().zip(n.into_iter()).collect::<BTreeMap<String, Number>>())
			}
		}
	}
}

impl From<(Number, Variables)> for Expr {
	fn from((c, v): (Number, Variables)) -> Self{

			Expr{
			a: None,
			b: None,
			op: EnumExpr::PARAM,
			x: Term {
				coefficient: c,
				variables: v
			}
		}
	}
}

impl From<HashMap<String, Mono>> for Expr{
	fn from(mono_map: HashMap<String, Mono>) -> Self {
		let mut exprs: Vec<Expr> = Vec::new();
		for mono in mono_map.values() { 
			let mono_expr: Expr = mono.e();
			exprs.push(mono_expr);
		}

		let mut result_expr: Expr;
		match exprs.len() {
			0 => {
				result_expr = Expr::from(0.0);
			}
			1 => {
				result_expr = exprs[0].clone();
			}
			_ => {
				result_expr = Expr::from(exprs[0].clone());
				for i in 1..exprs.len() {
					result_expr = result_expr.Plus(exprs[i].clone());
				}
			}
		}
		return result_expr;
	}
}

impl From<BTreeMap<String, Mono>> for Expr{
	fn from(mono_map: BTreeMap<String, Mono>) -> Self {
		let mut exprs: Vec<Expr> = Vec::new();
		for mono in mono_map.values() { 
			let mono_expr: Expr = mono.e();
			exprs.push(mono_expr);
		}

		let mut result_expr: Expr;
		match exprs.len() {
			0 => {
				result_expr = Expr::from(0.0);
			}
			1 => {
				result_expr = exprs[0].clone();
			}
			_ => {
				result_expr = Expr::from(exprs[0].clone());
				for i in 1..exprs.len() {
					result_expr = result_expr.Plus(exprs[i].clone());
				}
			}
		}
		return result_expr;
	}
}

impl Expr {
	pub fn a(&self) ->  Option<Arc<Expr>> {
		self.a.clone()
	}

	pub fn b(&self) ->  Option<Arc<Expr>> {
		self.b.clone()
	}

	pub fn config_mono(&self) -> Term {

		let recurse = |mut xterm: Term| -> Term  {
			if self.op == EnumExpr::CONSTANT {
				if xterm.coefficient == 1.0 {
					xterm.coefficient = self.x.coefficient.clone();
				}
				else {
					xterm.coefficient *= self.x.coefficient.clone();
				}
				return xterm
			}
			else if self.op == EnumExpr::PARAM {
				xterm.variables.extend(self.x.variables.clone());

				if xterm.coefficient == 1.0 {
					xterm.coefficient = self.x.coefficient.clone();
				}

				else {
					xterm.coefficient *= self.x.coefficient.clone();
				}
				return xterm;
			}
			else if self.op == EnumExpr::POW {
				let base: Arc<Expr> = self.a.clone().unwrap();
				let exponent: Arc<Expr> = self.b.clone().unwrap();

				if base.op == EnumExpr::PARAM && exponent.op == EnumExpr::CONSTANT {
					xterm.coefficient *= 1.0;
					xterm.variables.extend(base.x.variables.clone());

					if xterm.coefficient == 1.0 {
						xterm.coefficient = Number::Decimal(1.0);
					}
					else {
						xterm.coefficient *= 1.0;
					}
					return xterm;
				}
				else {
					panic!("Base must be a parameter and exponent must be a constant");
				}
			}
			else if self.op == EnumExpr::TIMES {
				let _left: Arc<Expr> = self.a.clone().unwrap();
				let right: Arc<Expr> = self.b.clone().unwrap();

				let l_term: Term = right.config_mono();
				let r_term = right.config_mono();

				xterm.variables.extend(l_term.variables);
				xterm.variables.extend(r_term.variables);

				xterm.coefficient += l_term.coefficient + r_term.coefficient;
				xterm
			}
			else {
				panic!("Unsupported operation in monomial conversion");
			}
		};

		let xterm: Term = Default::default();
		recurse(xterm)
		// return xterm;
	}

    pub fn children (&self) -> usize {
		match self.op {
			EnumExpr::CONSTANT => 0,
			EnumExpr::PARAM => 0,
			EnumExpr::POINT => 0,
			EnumExpr::ENTITY => 0,
			EnumExpr::PLUS => 2,
			EnumExpr::MINUS => 2,
			EnumExpr::TIMES => 2,
			EnumExpr::DIV => 2,
			EnumExpr::NEGATE => 1,
			EnumExpr::SQRT => 1,
			EnumExpr::SQUARE => 1,
			EnumExpr::SIN => 1,
			EnumExpr::COS => 1,
			EnumExpr::ASIN => 1,
			EnumExpr::ACOS => 1,
			_ => panic!("Unknown operation"),
		}
    }

    pub fn nodes (&self) -> usize {
        match self.children() {
            0 => 1,
            1 => 1 + self.a.clone().unwrap().nodes(),
            2 => 1 + self.a.clone().unwrap().nodes() + &self.b.clone().unwrap().nodes(),
            _ => panic!("Unknown operation")
        }
    }

	pub fn monomial_gme(&self)-> bool {
		// Implementation for monomial_gme
		if self.op == EnumExpr::CONSTANT || self.op == EnumExpr::PARAM {
			return true;
		}
		else if self.op == EnumExpr::POW {
			// base is the basis of the term
			// exponent is the power of the term
			// here we clone the Arc reference to a shared reference and unwrap the value
			// recall unwrap is always used last.
			
			let base: Arc<Expr> = self.a.clone().unwrap();
			let exponent: Arc<Expr> = self.b.clone().unwrap();

			if base.op == EnumExpr::CONSTANT && exponent.op == EnumExpr::PARAM {
				return true;
			}
			else {
				return false;
			}
		}
		else if self.op == EnumExpr::TIMES {
			let left: bool = self.a.clone().unwrap().monomial_gme();
			let right: bool = self.b.clone().unwrap().monomial_gme();
			return left && right;
		}
		else {
			return false;
		}
	}

	pub fn polynomial_gme(&self) -> Option<HashMap<String, Mono>> {
		// Implementation for polynomial_gme
		if self.monomial_gme() {
			let mut poly_map: HashMap<String, Mono> = HashMap::new();
			poly_map.insert(self.to_string(), Mono::from((self.clone(), Some(self.to_string()))));
			return Some(poly_map);
		}
		else {
			if self.op == EnumExpr::PLUS || self.op == EnumExpr::MINUS {
				let left: Option<HashMap<String, Mono>> = self.a.as_ref().unwrap().polynomial_gme();
				let right: Option<HashMap<String, Mono>> = self.b.as_ref().unwrap().polynomial_gme();

				let mut result: HashMap<String, Mono> = HashMap::new();

				match left {
					Some(val) => result.extend(val),
					_ => return None,
				}

				match right {
					Some(val) => result.extend(val),
					_ => return None,
				}

				return Some(result);
			}
			else {
				return None;
			}
		}
	}

	fn AnyBinOp(self, op: EnumExpr, b: Expr) -> Expr {
		let a: Arc<Expr> = Arc::new(self);
		let b = Arc::new(b);
		let marker = op;
		Self {
			a: Some(a),
			b: Some(b),
			op: marker,
			..Default::default()
		}
	}

	fn AnyUnaOp(self, op: EnumExpr) -> Expr {
		let a: Arc<Expr> = Arc::new(self);
		let marker = op;
		Self {
			a: Some(a),
			op: marker,
			..Default::default()
		}
	}

	pub fn Plus(self, b: Expr) -> Expr {
		return self.AnyBinOp(EnumExpr::PLUS, b);
	}

	pub fn Minus(self, b: Expr) -> Expr {
		return self.AnyBinOp(EnumExpr::MINUS, b);
	}

	pub fn Times (self, b: Expr) -> Expr {
		return self.AnyBinOp(EnumExpr::TIMES, b);
	}

	pub fn Div (self, b: Expr) -> Expr {
		return self.AnyBinOp(EnumExpr::DIV, b);
	}

	pub fn Negate (self) -> Expr {
		return self.AnyUnaOp(EnumExpr::NEGATE);
	}

	pub fn Sqrt (self) -> Expr {
		return self.AnyUnaOp(EnumExpr::SQRT);
	}

	pub fn Square (self) -> Expr {
		return self.AnyUnaOp(EnumExpr::SQUARE);
	}

	pub fn Sin (self) -> Expr {
		return self.AnyUnaOp(EnumExpr::SIN);
	}

	pub fn Cos (self) -> Expr {
		return self.AnyUnaOp(EnumExpr::COS);
	}

	pub fn ASin (self) -> Expr {
		return self.AnyUnaOp(EnumExpr::ASIN);
	}

	pub fn ACos (self) -> Expr {
		return self.AnyUnaOp(EnumExpr::ACOS);
	}

	// fn PartialWrt (self, p: String) -> Expr {
	// 	let da: Expr; = Expr::default(); // default should be 0.0
	// 	let db: = Expr::default();

	// 	match self.children() {
	// 		0 => match self.op {
	// 			EnumExpr::CONSTANT => {
	// 				return Expr::new(XExpr::v((0.0)));
	// 			}
	// 			EnumExpr::PARAM => {
	// 				if self.x == p {
	// 					return Expr::new(XExpr::v((1.0)));
	// 				}
	// 				else {
	// 					return Expr::new(XExpr::v((0.0)));
	// 				}
	// 			}
	// 			_ => panic!("Unknown operation"),
	// 		}
	// 		1 => match self.op {
	// 			EnumExpr::SIN =>
	// 				return da.Times(self.a.Cos()),
	// 			EnumExpr::
	// 			EnumExpr::PARAM => {
	// 				if self.x == p {
	// 					return Expr::new(XExpr::v((1.0)));
	// 				}
	// 				else {
	// 					return Expr::new(XExpr::v((0.0)));
	// 				}
	// 			}
	// 			_ => panic!("Unknown operation"),
	// 		}
	// 		2 => match self.op {
	// 			enum_Expr::CONSTANT => {
	// 				return Expr::default(0.0);
	// 			}
	// 			enum_Expr::PARAM => {
	// 				if self.x.parh == p {
	// 					return Expr::default(1.0);
	// 				}
	// 				else {
	// 					return Expr::default(0.0);
	// 				}
	// 			}
	// 			_ => {
	// 				panic!("Unknown operation"),
	// 			}
	// 		}
	// 		_ => panic!("Unknown operation"),
	// 	}
	// }	

	pub fn Eval (&self, values :&Variables) -> Number {
		match self.op {
			EnumExpr::CONSTANT => self.x.coefficient.clone(),
			EnumExpr::PARAM => {
				let mut result: Number = Number::Decimal(1.0);
				for (key, deg) in self.x.variables.iter() {
					match deg {
						Number::Decimal(exp) => match values.get(key) {
							Some(Number::Decimal(val)) => {result *= val.pow(exp);}
							_ => {result *= 0.0;}
						},
						_ => panic!("not implemented deg as other thing")
					}
				}
				result *= self.x.coefficient.clone();
				result
			}
			EnumExpr::PLUS => self.a.as_ref().unwrap().Eval(&values),
			EnumExpr::MINUS => self.a.as_ref().unwrap().Eval(&values) - self.b.as_ref().unwrap().Eval(&values),
			EnumExpr::TIMES => self.a.as_ref().unwrap().Eval(&values) * self.a.as_ref().unwrap().Eval(&values),
			EnumExpr::DIV => self.a.as_ref().unwrap().Eval(&values) / self.b.as_ref().unwrap().Eval(&values),
			EnumExpr::NEGATE => -self.a.as_ref().unwrap().Eval(&values),
			EnumExpr::SQRT => self.a.as_ref().unwrap().Eval(&values).sqrt(),
			EnumExpr::SQUARE => self.a.as_ref().unwrap().Eval(&values).powi(2),
			EnumExpr::SIN => self.a.as_ref().unwrap().Eval(&values).sin(),
			EnumExpr::COS => self.a.as_ref().unwrap().Eval(&values).cos(),
			EnumExpr::ASIN => self.a.as_ref().unwrap().Eval(&values).asin(),
			EnumExpr::ACOS => self.a.as_ref().unwrap().Eval(&values).acos(),
			_ => panic!("Unknown operation"),
		}
	}

	pub fn op(&self) -> EnumExpr {
		self.op.clone()
	}

	pub fn Tol(self, _a: f64, _b: f64) -> bool {
		todo!();
	}

}