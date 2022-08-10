use std::{iter::Sum, sync::Arc};

use isotest::*;

trait Common {
    fn num(&self) -> u8;
}

#[derive(Copy, Clone, Debug, derive_more::Add, derive_more::Sum)]
struct A(u8);

#[derive(Copy, Clone, Debug, derive_more::Add, derive_more::Sum)]
struct B(u8, u8);

impl Runners<A> for A {}
impl Runners<A> for B {}
// impl Runners<A> for Arc<A> {}
// impl Runners<A> for Arc<B> {}

impl Isotest for A {
    type Super = B;

    fn expand(&self) -> B {
        B(self.0, 0)
    }

    fn condense(big: B) -> Self {
        Self(big.0)
    }
}

impl Common for A {
    fn num(&self) -> u8 {
        self.0
    }
}

impl Common for B {
    fn num(&self) -> u8 {
        self.0
    }
}

impl<T: Common + ?Sized> Common for Arc<T> {
    fn num(&self) -> u8 {
        (**self).num()
    }
}

impl Common for Arc<dyn Runners<A>> {
    fn num(&self) -> u8 {
        (self.clone()).as_iso().num()
    }
}

fn process<T: Common, I: Iterator<Item = T>>(ts: I) -> u8 {
    ts.map(|t| Common::num(&t)).sum()
}

#[test]
fn basic() {
    // create: A -> X
    // update: (A -> A) -> (X -> X)

    run(|create, update| {
        let x = create(A(1));
        let y = create(A(2));
        let z = create(A(3));
        assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 6);

        let y = update(
            y,
            Box::new(|mut y: A| {
                y.0 = 4;
                y
            }),
        );
        assert_eq!(process([x, y, z].into_iter()), 8);
    });
}
