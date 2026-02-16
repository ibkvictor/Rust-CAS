pub mod expr;
pub mod poly;
pub mod mono;
pub mod solv;
pub mod ring;


pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;
    use expr::expr::*;
    use mono::mono::*;

    #[test]
    fn it_works() {
        let a: Expr = Expr::from(5.0);
        let _b: Expr = Expr::from("t");
        // a.clone();
        a.monomial_gme();
        a.nodes();
        a.children();
        let c: Mono = Mono::default();
        c.coefficient();
        let c: Expr = Expr::from(6.0);
        println!("a: {a}");
        println!("c: {c}");
        println!("a + c: {}", a.clone().Plus(c.clone()));
        println!("a - c: {}", a.clone().Minus(c.clone()));
        println!("a * c: {}", a.clone().Times(c.clone()));
        println!("a / c: {}", a.clone().Div(c.clone()));
        println!("a Negate: {}", a.clone().Negate());
        println!("a ops: {}", a.clone().op());
    }
}
