//! Proof-of-concept of types

trait X {
    type A: Clone + X<A = Self::A, B = Self::B>;
    type B: Clone + X<A = Self::A, B = Self::B>;

    fn a(&self) -> Self::A;
}

#[derive(Clone)]
struct A;

#[derive(Clone)]
struct B(A);

impl X for A {
    type A = A;
    type B = B;

    fn a(&self) -> Self::A {
        self.clone()
    }
}

impl X for B {
    type A = A;
    type B = B;

    fn a(&self) -> Self::A {
        self.0.clone()
    }
}

fn run<A, B>(x: Box<dyn X<A = A, B = B>>) -> A
where
    A: Clone + X<A = A, B = B>,
    B: Clone + X<A = A, B = B>,
{
    (*x).a()
}

fn main() {
    let a = A;
    let b = B(a.clone());
    run(Box::new(a));
    run(Box::new(b));
}
