use std::{any::Any, sync::Arc};

#[macro_export]
macro_rules! isotest {
    ($a:ty => $forward:expr, $b:ty => $backward:expr $(,)?) => {
        impl $crate::Isotest for $a {
            type Super = $b;

            fn forward(self) -> $b {
                let f: Box<dyn Fn($a) -> $b> = Box::new($forward);
                f(self)
            }

            fn backward(b: $b) -> Self {
                let f: Box<dyn Fn($b) -> $a> = Box::new($backward);
                f(b)
            }
        }

        impl $crate::Runners<$a> for $a {}
        impl $crate::Runners<$a> for $b {}
    };
}

pub trait Isotest: 'static + Clone + Runners<Self> {
    type Super: Clone + Runners<Self>;

    fn forward(self) -> Self::Super;
    fn backward(b: Self::Super) -> Self;
}

pub trait Runners<Iso: Isotest + Clone + 'static>: std::fmt::Debug + 'static {
    fn as_iso(self: Arc<Self>) -> Iso {
        let any: &dyn Any = &self;
        if let Some(iso) = any.downcast_ref::<Arc<Iso>>() {
            (*(iso.clone())).clone()
        } else if let Some(iso) = any.downcast_ref::<Arc<Iso::Super>>() {
            Iso::backward((*(iso.clone())).clone())
        } else {
            panic!("item is not of either expected Isotest type")
        }
    }
}

/// The main dynamic generic type produced by isotest.
/// You must implement your trait on `Ambi<YourTestStruct>`
pub type Ambi<T> = std::sync::Arc<dyn Runners<T>>;

fn generate1<Iso: Isotest + 'static>(x: Iso) -> Ambi<Iso> {
    Arc::new(x)
}

fn generate2<Iso: Isotest + 'static>(x: Iso) -> Ambi<Iso> {
    Arc::new(x.forward())
}

fn modify1<I, M>(x: Ambi<I>, f: M) -> Ambi<I>
where
    I: Isotest + 'static,
    M: Fn(I) -> I,
{
    Arc::new(f(x.as_iso()))
}

fn modify2<I, M>(x: Ambi<I>, f: M) -> Ambi<I>
where
    I: Isotest + 'static,
    M: Fn(I) -> I,
{
    // Self::generate1(f(*self))
    Arc::new(f(x.as_iso()).forward())
}

type Create<T> = Box<dyn Fn(T) -> Ambi<T>>;
type Update<T, M> = Box<dyn Fn(Ambi<T>, M) -> Arc<dyn Runners<T>>>;

pub fn run<I, M>(runner: impl Fn(Create<I>, Update<I, M>))
where
    I: Isotest + 'static,
    M: Fn(I) -> I + 'static,
{
    runner(Box::new(generate1), Box::new(modify1));
    runner(Box::new(generate2), Box::new(modify2));
}
