use std::ops::*;
use std::cmp::*;
use std::collections::*;
use num_notation::*;
use ndarray::*;
use std::fmt::{Display};
use std::result::*;

use crate::expr::expr::*;

#[derive(Clone, Debug)]
// Mono versus term, mono provides ordering of polynomial ordering of variables
pub struct Mono {
	coefficient: Number,
	variables: Variables,
    ORDER: (Option<String>, bool), // what do you need bool for?
    e: Option<Expr>
}

// beware do not call default inside itself else stack overflow occurs.
impl Default for Mono {
    fn default() -> Self {
        Mono { 
            coefficient: Number::Decimal(0.0),
            variables: BTreeMap::new(),
            ORDER: (None, false),
            e: None
        }
    }
}

impl PartialEq for Mono {
    fn eq(&self, other: &Self) -> bool {
        self.variables == other.variables
    }
}

impl Display for Mono {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.e.clone().unwrap())
    }
}

impl PartialOrd for Mono {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        assert_eq!(self.ORDER.0, other.ORDER.0);
        let array_op1: Array1<Number> = Array1::from_vec(self.variables().values().cloned().collect());
        let array_op2: Array1<Number> = Array1::from_vec(other.variables().values().cloned().collect());
        let diff: Array1<Number> = array_op1 - array_op2;
        match self.ORDER.0.clone() {
            Some(val) => {
                match val.as_str() {
                    "LEX" => {
                        let mut result: Option<Ordering> = None;
                        for iter in diff.iter() {
                            // if alpha_1 is equal to beta_1 continue else
                            if *iter == 0 {
                                continue;
                            }
                            else if *iter > 0 {
                                result = Some(Ordering::Greater);
                            }
                            else {
                                result = Some(Ordering::Less);
                            }
                        }
                        result
                    },
                    "REVGRADLEX" => {
                        let mut result: Option<Ordering> = None;
                        for arr_iter in diff.iter().rev() {
                            // if alpha_1 is equal to beta_1 continue else
                            if *arr_iter == 0 {
                                continue;
                            }
                            else if *arr_iter > 0 {
                                result = Some(Ordering::Greater);
                            }
                            else {
                                result = Some(Ordering::Less);
                            }
                        }
                        result
                    }
                    _ => panic!("order case is not implemented: use one of REVGRADLEX OR LEX")
                }
            },
            None => panic!("ordering not provide and default ordering has been removed. Kindly set ordering")
        }
    }
}

impl From<(Expr, Option<String>)> for Mono {
    fn from((e, order): (Expr, Option<String>)) -> Self{
        let xterm: Term = e.config_mono();
        // What is the difference between a term and a monomial
        // a term has an order and a monomial does not.
        Mono { 
            coefficient: xterm.coefficient().clone(),
            variables: xterm.variables().clone(),
            ORDER: (order, true),
            e: Some(e),
        }
    }
}

impl <'a, 'b> Mul<&'b Mono> for &'a Mono {
    type Output = Result<Mono, bool>;

    fn mul(self, other: &'b Mono) -> Self::Output {
        let (mutable_self, mutable_other) = self.configure(other);
        let vec1: Array1<Number> = Array1::from(self.variables.values().cloned().collect::<Vec<_>>());
        let vec2: Array1<Number> = Array1::from(other.variables.values().cloned().collect::<Vec<_>>());

        let vec: Array1<Number> = vec1 + vec2;
        let k: Vec<String>  = mutable_self.variables.keys().cloned().collect();
        let (vals, _) = vec.into_raw_vec_and_offset();

        let coeff = mutable_self.coefficient().clone() * mutable_other.coefficient().clone();
        let tmp_bmap: BTreeMap<String, Number> = k.iter().zip(vals.iter()).map(|(key, val)| (key.clone(), val.clone())).collect();
        
        let e: Expr = Expr::from((coeff.clone(), k, vals));

        return Ok(Mono {
            coefficient: coeff,
            variables: tmp_bmap,
            ORDER: mutable_self.ORDER,
            e: Some(e),
        });
    }
}

impl <'a, 'b> Div<&'b Mono> for &'a Mono {
    type Output = Result<Mono, bool>;

    fn div(self, other: &'b Mono) -> Self::Output {
        let (mutable_self, mutable_other) = self.configure(other);
        let vec1: Array1<Number> = Array1::from(self.variables.values().cloned().collect::<Vec<_>>());
        let vec2: Array1<Number> = Array1::from(other.variables.values().cloned().collect::<Vec<_>>());

        let vec: Array1<Number> = vec1 - vec2;
        let k: Vec<String>  = mutable_self.variables.keys().cloned().collect();
        let (vals, _) = vec.into_raw_vec_and_offset();

        let coeff = mutable_self.coefficient().clone() / mutable_other.coefficient().clone();
        let tmp_bmap: BTreeMap<String, Number> = k.iter().zip(vals.iter()).map(|(key, val)| (key.clone(), val.clone())).collect();

        let e: Expr = Expr::from((coeff.clone(), Vec::from(k), vals));

        return Ok(Mono {
            coefficient: coeff,
            variables: tmp_bmap,
            ORDER: mutable_self.ORDER,
            e: Some(e),
        });
    }
}

impl Div for Mono {
    type Output = Result<Mono, bool>;

    fn div(self, other: Mono) -> Self::Output {
        let (mutable_self, mutable_other) = self.configure(&other);
        let vec1: Array1<Number> = Array1::from(self.variables.values().cloned().collect::<Vec<_>>());
        let vec2: Array1<Number> = Array1::from(other.variables.values().cloned().collect::<Vec<_>>());

        let vec: Array1<Number> = vec1 - vec2;
        let k: Vec<String>  = mutable_self.variables.keys().cloned().collect();
        let (vals, _) = vec.into_raw_vec_and_offset();

        let coeff = mutable_self.coefficient().clone() / mutable_other.coefficient().clone();
        let tmp_bmap: BTreeMap<String, Number> = k.iter().zip(vals.iter()).map(|(key, val)| (key.clone(), val.clone())).collect();

        let e: Expr = Expr::from((coeff.clone(), Vec::from(k), vals));

        return Ok(Mono {
            coefficient: coeff,
            variables: tmp_bmap,
            ORDER: mutable_self.ORDER,
            e: Some(e),
        });
    }
}

impl <'a, 'b> Add<&'b Mono> for &'a Mono {
    type Output = Result<Mono, bool>;

    fn add(self, other: &'b Mono) -> Self::Output {
        // common terms
        let (new_self, new_other): (Mono, Mono) = self.configure(&other);

        if new_self.common_term(&new_other) {
            let c: Number = new_self.coefficient().clone() + new_other.coefficient().clone();
            let e: Expr = Expr::from((c.clone(), new_self.variables.keys().cloned().collect::<Vec<_>>(), new_self.variables.values().cloned().collect::<Vec<_>>()));
            return Ok(Mono {
                coefficient: c,
                variables: new_self.variables().clone(),
                ORDER: new_self.ORDER,
                e: Some(e)
            });
        }
        else {
            return Err(false);
        }
    }
}

impl <'a, 'b> Sub<&'b Mono> for &'a Mono {
    type Output = Result<Mono, bool>;

    fn sub(self, other: &'b Mono) -> Self::Output {
        // common terms
        let (new_self, new_other): (Mono, Mono) = self.configure(&other);

        if new_self.common_term(&new_other) {
            let c: Number = new_self.coefficient().clone() - new_other.coefficient().clone();
            let e: Expr = Expr::from((c.clone(), new_self.variables.keys().cloned().collect::<Vec<_>>(), new_self.variables.values().cloned().collect::<Vec<_>>()));
            return Ok(Mono {
                coefficient: c,
                variables: new_self.variables().clone(),
                ORDER: new_self.ORDER,
                e: Some(e)
            });
        }
        else {
            return Err(false);
        }
    }
}

impl Mono {
    pub fn e(&self) -> Expr {
        self.e.clone().unwrap()
    }
    pub fn order(&mut self, val: String){
        self.ORDER.0 = Some(val);
    }

    pub fn coefficient (&self) -> &Number {
        return &self.coefficient;
    }

    pub fn coefficient_mut (&self) -> Number {
        let result = self.coefficient().clone();
        result
    }

    pub fn variables (&self) -> &BTreeMap<String, Number> {
        return &self.variables;
    }

    pub fn variables_mut (&self) -> BTreeMap<String, Number> {
        let result = self.variables().clone();
        result
    }

    pub fn configure(&self, other: &Mono) -> (Mono, Mono) {
        let mut keys: BTreeSet<String> = BTreeSet::new();
        let mut mutable_self = Mono::default();
        let mut mutable_other: Mono = Mono::default();

        mutable_self.coefficient = self.coefficient.clone();
        mutable_other.coefficient = other.coefficient.clone();

        for self_ch in self.variables.keys() {
            keys.insert(self_ch.clone());
        }

        for other_ch in other.variables.keys() {
            keys.insert(other_ch.clone());
        }

        for k in keys.iter()  {
            if !mutable_self.variables.contains_key(k){
                match self.variables.contains_key(k) {
                    true => {
                        mutable_self.variables.insert(k.clone(), self.variables[k.as_str()].clone());
                    }
                    false => {
                        mutable_self.variables.insert(k.clone(), Number::Decimal(0.0));
                    }          
                } 
            }

            if !mutable_other.variables.contains_key(k){
                match other.variables.contains_key(k) {
                    true => {
                        mutable_other.variables.insert(k.clone(), other.variables[k].clone());
                    }
                    false => {
                        mutable_other.variables.insert(k.clone(), Number::Decimal(0.0));
                    }     
                }
            }
        }

        return (mutable_self, mutable_other);

    }

    pub fn cofactor(&self, other: &Mono) -> Result<Mono, bool> {
        if self.variables.keys().all(|key| other.variables.contains_key(key)) {
            self.configure(other);
        }
        let vars = self.variables.keys();
        let deg1: Array1<Number> = Array1::from(self.variables.values().cloned().collect::<Vec<_>>());
        let deg2: Array1<Number> = Array1::from(other.variables.values().cloned().collect::<Vec<_>>());
        let diff: Array1<Number> = deg2 - deg1;
        let d_coef: Number = other.coefficient().clone() / self.coefficient().clone();

        for val in diff.iter() {
            if *val < 0 {
                return Err(false);
            }
        }
        let k: Vec<String>  = vars.cloned().collect();
        let (vals, _) = diff.into_raw_vec_and_offset();
        let tmp_bmap: BTreeMap<String, Number> = k.iter().zip(vals.iter()).map(|(key, val)| (key.clone(), val.clone())).collect();
        
        let _e: Expr = Expr::from((d_coef.clone(), k, vals));

        Ok(Mono {
            coefficient: d_coef,
            variables: tmp_bmap,
            ORDER: self.ORDER.clone(),
            e: None
        })
    }

    pub fn common_term(&self, other: &Mono) -> bool {
        match self.variables == other.variables {
            true => {
                        let a: Array1<Number> = Array1::from(self.variables().values().cloned().collect::<Vec<_>>());
                        let b: Array1<Number> = Array1::from(other.variables().values().cloned().collect::<Vec<_>>());
                        if a == b {
                            return true;
                        }
                        return false;
                },
            false => return false
        }
    }

    pub fn update_expr(&mut self) {
        let tmp_mono = self.clone();
        self.e = Some(Expr::from((tmp_mono.coefficient, tmp_mono.variables))); //only mut self becuase muting e is important
    }

    pub fn remove_zero(&mut self) -> bool {
        let mut rm_entry = None;
        let mut tmp_variables = self.variables.clone();
        for (key, _val) in self.variables.iter_mut() {
            match tmp_variables.remove(&key.clone()) {
                Some(val) => { 
                    rm_entry = Some(val);
                },
                None => ()
            }
        }
        //update without multiple mutability
        self.variables = tmp_variables;
        match rm_entry {
            Some(_val) => true,
            None => false
        }
    }

    // also updates by zeroing monomials
    pub fn zero(&mut self) -> bool {
        self.remove_zero();
        if self.coefficient != Number::Decimal(0.0) {
            return false;
        } else {
            for (_, val) in self.variables.iter() {
                if *val != Number::Decimal(0.0) {
                    return false;
                }
            }
        }
        true
    }

    pub fn lcm(a: &Mono, b: &Mono) -> Mono {
        let mut keys: BTreeSet<String> = BTreeSet::new();
        let mut lcm_mono: Mono = Mono::default();
        for self_ch in a.variables.keys() {
            keys.insert(self_ch.clone());
        }
        for other_ch in b.variables.keys() {
            keys.insert(other_ch.clone());
        }
        for k in keys.iter() {
            let deg_a: Number = match a.variables.contains_key(k) {
                true => a.variables[k].clone(),
                false => Number::Decimal(0.0)
            };
            let deg_b: Number = match b.variables.contains_key(k) {
                true => b.variables[k].clone(),
                false => Number::Decimal(0.0)
            };
            lcm_mono.variables.insert(k.clone(), deg_a.max(deg_b));
        }
        lcm_mono.coefficient = Number::Decimal(1.0);
        lcm_mono.ORDER = a.ORDER.clone();
        lcm_mono.update_expr();
        lcm_mono
    }
}