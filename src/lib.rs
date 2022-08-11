use std::{any::Any, sync::Arc};

#[macro_export]
macro_rules! isotest {
    ($a:ty : $forward:expr, $b:ty : $backward:expr $(,)?) => {
        impl Isotest for $a {
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

        impl Runners<$a> for $a {}
        impl Runners<$a> for $b {}
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
pub type Ambi<T> = Arc<dyn Runners<T>>;

fn generate1<Iso: Isotest + 'static>(x: Iso) -> Ambi<Iso> {
    Arc::new(x)
}

fn generate2<Iso: Isotest + 'static>(x: Iso) -> Ambi<Iso> {
    Arc::new(x.forward())
}

fn modify1<Iso: Isotest + 'static>(x: Ambi<Iso>, f: Mutation<Iso>) -> Ambi<Iso> {
    Arc::new(f(x.as_iso()))
}

fn modify2<Iso: Isotest + 'static>(x: Ambi<Iso>, f: Mutation<Iso>) -> Ambi<Iso> {
    // Self::generate1(f(*self))
    Arc::new(f(x.as_iso()).forward())
}

type Mutation<T> = Box<dyn Fn(T) -> T>;

type Create<T> = Box<dyn Fn(T) -> Ambi<T>>;
type Update<T> = Box<dyn Fn(Ambi<T>, Mutation<T>) -> Arc<dyn Runners<T>>>;

pub fn run<I: Isotest + 'static>(runner: impl Fn(Create<I>, Update<I>)) {
    runner(Box::new(generate1), Box::new(modify1));
    runner(Box::new(generate2), Box::new(modify2));
}
