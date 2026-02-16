use num_notation::*;
use std::collections::*;
use std::hash::*;
use std::ops::*; // used for index trait
 // for TypeId
use std::fmt::{Display, Formatter};

use crate::mono::mono::*;
use crate::expr::expr::*;

pub struct Poly {
    ORDER: (String, bool),
    monomials: BTreeMap<String, Mono>, // if the Mono are transient during calculations how efficient is the hash of their exp as opposed to using a standard hashing function
    e: Expr,
}

impl Default for Poly {
    fn default() -> Self {
        Self {
            ORDER: ("LEV".to_string(), true),
            monomials: BTreeMap::new(),
            e: Expr::default(),
        }
    }   
}

impl From<Expr> for Poly{
    fn from(e: Expr) -> Self {
        Self {
            ORDER: ("LEV".to_string(), true),
            monomials: BTreeMap::new(),
            e: e,
        }
    }
}

impl From<HashMap<String, Expr>> for Poly {
    fn from(map: HashMap<String, Expr>) -> Self {
        let mut mono_map: HashMap<String, Mono> = HashMap::new();
        for (_, mono_expr) in map.iter() {
            let mono_expr_clone = mono_expr.clone();
            let mono_string: String = mono_expr_clone.to_string();
            let tmp_mono: Mono = Mono::from((mono_expr_clone, Some(mono_string.clone())));
            mono_map.insert(mono_string, tmp_mono);
        }
        let e: Expr = Expr::from(mono_map.clone());
        Self {
            ORDER: ("GLEV".to_string(), true),
            monomials: mono_map.into_iter().collect(),
            e: e,
        }
    }
}

impl From<Mono> for Poly{  
    fn from(mono: Mono) -> Self {
        let e: Expr = Expr::from(mono.clone());
        Self {
            ORDER: ("GLEV".to_string(), true),
            monomials: BTreeMap::from([
                (mono.clone().to_string(), mono)
            ]),
            e: e,
        }
    }
}

impl From<BTreeMap<String, Mono>> for Poly{  
    fn from(map: BTreeMap<String, Mono>) -> Self {
        let e: Expr = Expr::from(map.clone());
        Self {
            ORDER: ("GLEV".to_string(), true),
            monomials: map.into_iter().collect(),
            e: e,
        }
    }
}

impl Poly {
    // correct configure function // corrcted to mean an ordered polynomial with monomials as ring of all variables in the polynomial.
    pub fn configure(&mut self) {
        let mut keys: BTreeSet<String> = BTreeSet::new();
        for (_mono_key, mono_value) in self.monomials.iter() {
            for (variable_str, _variable_value) in mono_value.variables().iter() {
                keys.insert(variable_str.clone());
            } 
        }
        for k in keys.iter()  {
            for (_, mutable_mono) in self.monomials.iter_mut(){
                match mutable_mono.variables().contains_key(k) {
                    true => {},
                    false => {
                        mutable_mono.variables_mut().insert(k.clone(), Number::Decimal(0.0));
                    }
                }
            }
        }
    }

    pub fn kind(&self) -> EnumExpr {
        self.e.op()
    }

    pub fn operands(self) -> VecDeque<Expr> {
        let mut queue: VecDeque<Expr> = VecDeque::new();
        let mut result = VecDeque::new();
        queue.push_back(self.e.clone());
        result.push_back(self.e.clone());

        while !queue.is_empty() {
            let exp = queue.pop_front().unwrap();
            if !(exp.op() == EnumExpr::CONSTANT || exp.op() == EnumExpr::PARAM || exp.op() == EnumExpr::PARAM_PTR) {
                if exp.children() == 2 {
                    queue.push_back(exp.a().unwrap().as_ref().clone()); // unwrap panics if no element was found
                    result.push_back(exp.a().unwrap().as_ref().clone());
                    
                    queue.push_back(exp.b().unwrap().as_ref().clone());
                    result.push_back(exp.b().unwrap().as_ref().clone());
                } else if exp.children() == 1 {
                    queue.push_back(exp.a().unwrap().as_ref().clone());
                    result.push_back(exp.a().unwrap().as_ref().clone());
                }
            }
        }
        return result;
    }

    pub fn n_operands(self) -> usize {
        return self.e.nodes() as usize; // u64 converted to usize
    }

    pub fn i_operand(self, i: usize) -> Expr {
        let ops: VecDeque<Expr> = self.operands();
        return ops[i].clone();
    }

    pub fn monomial_gme(self) -> bool {
        return self.e.monomial_gme();
    }

    fn leading_term(&mut self) -> &Mono { //only mut self because order method is used here
        if self.ORDER.1 == false {
            return self.monomials.values().next().unwrap();
        }
        else {
            self.order(self.ORDER.0.clone().as_str());
            return self.monomials.values().next().unwrap();
        }
    }

    pub fn leading_coefficient(&mut self) -> Number { // only mut self because leading term is mut self
        let term: &Mono = self.leading_term();
        return term.coefficient().clone();
    }

    fn order(&mut self, val: &str) {

        fn divide(items: Vec<Mono>) -> Vec<Mono>  {
            match items.len() {
                0 | 1 => return items ,
                _ => {
                    let mid = items.len() / 2;
                    let left = items[..mid].to_vec();
                    let right = items[mid..].to_vec();
                    return merge_sort(left, right);
                }
            }
            
        }

        fn merge_sort(a: Vec<Mono>, b: Vec<Mono>) -> Vec<Mono> {
            let mut i: usize = 0;
            let mut j: usize = 0;

            let mut result: Vec<Mono> = Vec::new();
            while i < a.len() && j < b.len() {
                if a[i] <= b[j] {
                    result.push(a[i].clone());
                    i += 1;
                } else {
                    result.push(b[j].clone());
                    j += 1;
                }
            }

            if i < a.len() {
                result.extend_from_slice(&a[i..]);
            }
            if j < b.len() {
                result.extend_from_slice(&b[j..]);
            }
            return result;
        }

        self.ORDER.0 = val.to_string();
        for mono in self.monomials.values_mut() {
            mono.order(val.to_string());
        }

        let vec_mono = self.monomials.values().cloned().collect();
        let sorted_vec_mono = divide(vec_mono);

        for (mono, sorted_mono) in self.monomials.values_mut().zip(sorted_vec_mono.into_iter()) {
            *mono = sorted_mono;
        }

        self.ORDER.1 = true;
        
    }

    pub fn remove_zero(&mut self) -> bool {
        let mut rm_entry = None;
        let mut tmp_monomials = self.monomials.clone();
        for (key, _val) in self.monomials.iter_mut() {
            if tmp_monomials.get_mut(key).unwrap().zero() {
                match tmp_monomials.remove(&key.clone()) {
                    Some(val) => { 
                        rm_entry = Some(val);
                    },
                    None => ()
                }
            }
        }
        //update without multiple mutability
        self.monomials = tmp_monomials;
        match rm_entry {
            Some(_val) => true,
            None => false
        }
    }

    pub fn zero(&mut self) -> bool {
        self.remove_zero();
        if self.monomials.is_empty() {
            return true
        }
        else {
            return false
        }
    }

    pub fn update_expr(&mut self) {
        for (_, mono) in self.monomials.iter_mut() { //only mut self becuase muting e is important
            mono.update_expr();
        }
    }
}

impl Hash for Poly {
    // Note: No generic parameters for Self here
    fn hash<H: Hasher>(&self, state: &mut H) {
        let poly_expr = Expr::from(self.monomials.clone());
        poly_expr.hash(state); // Delegates to the hasher
    }
}

impl Display for Poly {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.e)
    }
}

impl Index<usize> for Poly {
    type Output = Mono;

    fn index(&self, index: usize) -> &Self::Output {
        self.monomials.values().collect::<Vec<_>>()[index]
    }
}

impl IndexMut<usize> for Poly {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.monomials.values_mut().nth(index).unwrap()
    }
}

impl PartialEq for Poly {
    fn eq(&self, other: &Self) -> bool {
        let _self_exp = Expr::from(self.monomials.clone());
        let _other_exp = Expr::from(other.monomials.clone());
        return self.e == other.e;
    }
}

impl Clone for Poly {
    fn clone(&self) -> Self {
        Poly {
            ORDER: self.ORDER.clone(),
            monomials: self.monomials.clone(),
            e: self.e.clone()
        }
    }
}

// implementations of multiplication operator for Poly
impl Mul<Mono> for Poly {
    type Output = Poly; 

    fn mul(self, other: Mono) -> Self::Output {
        let mut return_poly = self.clone();
        let other  = Mono::from(other);
        for mono in return_poly.monomials.values_mut() {
            *mono = mono.mul(&other).unwrap_or_else(|_| mono.clone());
        }
        return_poly.update_expr(); // update expr after multiplication
        return self;
    }
}

impl Mul<Poly> for Poly {
    type Output = Poly; 

    fn mul(self, other: Poly) -> Self::Output {
        let mut return_poly = self.clone();
        for mono1 in return_poly.monomials.values_mut() {
            for mono2 in other.monomials.values() {
                *mono1 = mono1.mul(mono2).unwrap_or_else(|_| mono1.clone());
            }
        }
        return_poly.update_expr(); // update expr after multiplication
        return self;
    }
}

impl Add<Mono> for Poly {
    type Output = Poly;
    fn add(self, rhs: Mono) -> Self::Output {
        let mut result_poly = self;
        let mut common_mono: Option<&Mono> = None;
        for mono in result_poly.monomials.values_mut() {
            if mono.common_term(&rhs) {
                common_mono = Some(mono);
            }                
        }

        match common_mono {
            Some(val) => {
                let mut coeff = val.coefficient_mut();
                coeff += rhs.coefficient().clone(); // be careful no setter used here
                let e: Expr = Expr::from(result_poly.monomials.clone());
                result_poly.e = e;
                result_poly
            }
            None => {
                result_poly.monomials.insert(rhs.to_string(), rhs);
                let e: Expr = Expr::from(result_poly.monomials.clone());
                result_poly.e = e;
                result_poly
            }
        }
        
    }
}

// implementations of addition operator for Poly
impl Add for Poly {
    type Output = Poly;

    fn add(self, other: Self) -> Self::Output {
        let mut result_poly = self.clone();
        for (_, mono) in other.monomials {
            result_poly = result_poly + mono;
        }
        result_poly
    }
}

// implementations of subtraction operator for Poly, Mono
impl Sub<Mono> for Poly {
    type Output = Poly;
    fn sub(self, rhs: Mono) -> Self::Output {
        let mut result_poly = self;
        let mut common_mono: Option<&Mono> = None;
        for mono in result_poly.monomials.values_mut() {
            if mono.common_term(&rhs) {
                common_mono = Some(mono);
            }                
        }

        match common_mono {
            Some(val) => {
                let mut coeff = val.coefficient_mut();
                coeff += rhs.coefficient().clone(); // be careful no setter used here
                let e: Expr = Expr::from(result_poly.monomials.clone());
                result_poly.e = e;
                result_poly
            }
            None => {
                result_poly.monomials.insert(rhs.to_string(), rhs);
                let e: Expr = Expr::from(result_poly.monomials.clone());
                result_poly.e = e;
                result_poly
            }
        }
        
    }
}

// implementations of subtraction operator for Poly, Poly
impl Sub for Poly {
    type Output = Poly;

    fn sub(self, other: Self) -> Self::Output {
        let mut result_poly = self.clone();
        for (_, mono) in other.monomials {
            result_poly = result_poly - mono;
        }
        result_poly
    }
}

// implementations of division operator for Poly
impl Div for Poly {
    type Output = (Poly, Poly);

    fn div(self, other: Poly) -> Self::Output {
        let mut other_vec: Vec<Poly> = Vec::new();
        other_vec.push(other);

        let mut p = self.clone();
        let mut g = other_vec.clone();
        let mut q = Poly::default();
        let mut r = Poly::default();

        let mut i: i32 = -1;
        while  !p.zero() {
            i += 1;
            let i = usize::try_from(i).expect("Failed to convert: value is negative or too large");

            // value of i is guarranteed to be usize for indexing
            let test = g[i].leading_term().cofactor(p.leading_term()).clone();
            match test.clone() {
                Ok(_value) => {
                    let lt_mult: Mono = p.leading_term().clone().div(&(g[i].leading_term().clone())).unwrap();
                    p = p - (g[i].clone() * Poly::from(lt_mult.clone()));
                    q = q + Poly::from(lt_mult.clone());
                }
                Err(_error) => ()
            }

            // now value of i is guarranteed to be i32 for subtraction
            let mut i: i32 = i32::try_from(i).expect("Failed to convert: value is negative or too large");
            match test.clone() {
                Ok(_value) => {
                    i = -1;
                }
                Err(_error) => ()
            }

            if i == ((other_vec.len() - 1) as i32) {
                let lt_p: Mono = p.leading_term().clone();
                r = r + Poly::from(lt_p.clone());
                p = p - Poly::from(lt_p.clone());
                i = -1;
            }
        }
        return (q, r);
    }
}

impl Div<Vec<Poly>> for Poly {
    type Output = (Poly, Poly);

    fn div(self, other: Vec<Poly>) -> Self::Output {
        let mut p = self.clone();
        let mut g = other.clone();
        let mut q = Poly::default();
        let mut r = Poly::default();

        let mut i: i32 = -1;
        while  !p.zero() {
            i += 1;
            let i = usize::try_from(i).expect("Failed to convert: value is negative or too large");

            // value of i is guarranteed to be usize for indexing
            let test = g[i].leading_term().cofactor(p.leading_term()).clone();
            match test.clone() {
                Ok(_value) => {
                    let lt_mult: Mono = p.leading_term().clone().div(&(g[i].leading_term().clone())).unwrap();
                    p = p - (g[i].clone() * Poly::from(lt_mult.clone()));
                    q = q + Poly::from(lt_mult.clone());
                }
                Err(_error) => ()
            }

            // now value of i is guarranteed to be i32 for subtraction
            let mut i: i32 = i32::try_from(i).expect("Failed to convert: value is negative or too large");
            match test.clone() {
                Ok(_value) => {
                    i = -1;
                }
                Err(_error) => ()
            }

            if i == ((other.len() - 1) as i32) {
                let lt_p: Mono = p.leading_term().clone();
                r = r + Poly::from(lt_p.clone());
                p = p - Poly::from(lt_p.clone());
                i = -1;
            }
        }
        return (q, r);
    }
}

impl Div<Mono> for Poly {
    type Output = Poly;

    fn div(self, other: Mono) -> Self::Output {
        let mut result_poly = self;
        let mut updated_monomials = BTreeMap::new();
        for (key, mono) in result_poly.monomials.iter() {
            let (new_mono, other_val) = mono.configure(&other);
            let divided_mono = new_mono.div(&other_val).unwrap();
            updated_monomials.insert(key.clone(), divided_mono);
        }
        result_poly.monomials = updated_monomials;
        result_poly.update_expr();
        result_poly
    }
}