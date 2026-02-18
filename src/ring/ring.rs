use std::collections::*;
use num_notation::Number;

use crate::mono::mono::*;
use crate::expr::expr::*;
use crate::poly::poly::Poly;

#[derive(Clone)]
pub struct Ring {
    pub variables: HashSet<String>,
    pub polynomials: Option<HashSet<Poly>>
}

impl From<&str> for Ring {
    fn from(_value: &str) -> Self {
        Ring {
            variables: HashSet::from([_value.to_string()]),
            polynomials: None
        }
    }
}

impl From<String> for Ring {
    fn from(_value: String) -> Self {
        Ring {
            variables: HashSet::from([_value]),
            polynomials: None
        }
    }
}

impl From<HashSet<String>> for Ring {
    fn from(_value: HashSet<String>) -> Self {
        Ring {
            variables: _value,
            polynomials: None
        }
    }
}

impl From<Ring> for HashSet<String> {
    fn from(_value: Ring) -> Self {
        _value.variables
    }
}

impl From<HashSet<Poly>> for Ring {
    fn from(_value: HashSet<Poly>) -> Self {
        let mut variables: HashSet<String> = HashSet::default();
        for poly in _value.iter() {
            for (_, mono) in poly.monomials().iter() {
                for (k, v) in mono.variables().iter() {
                    variables.insert(k.clone());
                }
            }
        }

        Ring {
            variables: variables,
            polynomials: Some(_value)
        }
    }
}

impl Default for Ring {
    fn default() -> Self {
        Ring { variables: HashSet::default(), polynomials: None }
    }
}

pub trait GrobnerBasis {
    fn static_s_poly(a: Poly, b: Poly) -> Poly;
    fn s_poly(&self, a: Poly, b: Poly) -> Poly;
    fn grobner_basis(&self, set_poly :HashSet<Poly>);
    fn static_reduced_grobner_basis(set_poly :HashSet<Poly>) -> HashSet<Poly>;
    fn reduced_grobner_basis(&self, set_poly :HashSet<Poly>);
    fn static_reduced_set(set_poly :HashSet<Poly>) -> HashSet<Poly>;
    fn reduced_set(&self, set_poly :HashSet<Poly>);
}

impl Ring {
    pub fn configure(&self, set_poly: HashSet<Poly>) -> HashSet<Poly> {
        let tmp_self: Ring = self.clone();
        let mut tmp_hash_map: BTreeMap<String, Number> = BTreeMap::new();
        
        for key in tmp_self.variables.into_iter(){
            tmp_hash_map.insert(key , Number::Decimal(0.0));
        }

        let tmp_exp: Expr = Expr::from((Number::Decimal(1.0), tmp_hash_map));
        let tmp_mono: Mono = Mono::from((tmp_exp, None));

        let tmp_set_poly: HashSet<Poly> = set_poly;

        for mut poly in tmp_set_poly.iter(){
            poly = &(poly.clone() * tmp_mono.clone());
        }

        tmp_set_poly

        // for mut poly in tmp_set_poly.into_iter() {
        //     for mut mono: Mono in poly.mono
        // }
    }

    fn s_poly(mut a: Poly, mut b: Poly) -> Poly {
        let a_lt_mono: &Mono = a.leading_term();
        let b_lt_mono: &Mono = b.leading_term();

        let L: Mono = Mono::lcm(&(a_lt_mono.clone()), &(b_lt_mono.clone()));

        let m: Mono = (L.clone() / a_lt_mono.clone()).unwrap();
        let m_prime: Mono = (L / b_lt_mono.clone()).unwrap();

        let s0 = a.clone() * m;
        let s1: Poly = b.clone() * m_prime;

        return s0 - s1
    }

    pub fn reduced_grobner_basis(set_poly :HashSet<Poly>) -> HashSet<Poly> {
        let mut tmp_set_poly: HashSet<Poly> = set_poly.clone();

        let mut i: usize = 0;
        let mut j: usize = 0;

        while i < tmp_set_poly.len() {
            while j < tmp_set_poly.len() {
                let vec_set_poly: Vec<Poly> = tmp_set_poly.iter().cloned().collect();
                if i != j {
                    let f: &Poly = &vec_set_poly[i];
                    let g: &Poly = &vec_set_poly[j];
                    let s: Poly = Ring::s_poly(f.clone(), g.clone());
                    let (_, mut r): (Poly, Poly) = s.clone() / vec_set_poly;

                    if !r.zero() {
                        tmp_set_poly.insert(s);
                        let tmp_set_poly_2 = Ring::reduced_set(tmp_set_poly.clone());

                        if tmp_set_poly != tmp_set_poly_2 {
                            tmp_set_poly = tmp_set_poly_2;
                            i -= 1;
                            j = 0;
                            break;
                        }
                    }
                }
                j += 1;
            }
            i += 1;
        }
        tmp_set_poly
    }

    pub fn reduced_set(set_poly :HashSet<Poly>) -> HashSet<Poly> {
        let mut i: usize = 0;
        let mut vec_set_poly: Vec<Poly> = set_poly.into_iter().collect(); 

        while i < vec_set_poly.len() {
            let mut temp_vec_set_poly: Vec<Poly> = vec_set_poly.clone();
            let f_ = temp_vec_set_poly.remove(i);
            let (q, mut r) = f_ / temp_vec_set_poly;
            if r.zero() {
                vec_set_poly.remove(i);
            }
            i += 1;
        }
        let mut result: HashSet<Poly> = HashSet::new();
        for p in vec_set_poly.into_iter() {
            result.insert(p);
        }
        result
    }
}
