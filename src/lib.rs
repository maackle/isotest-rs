use std::any::Any;
use std::sync::Arc;

pub trait Iso: 'static {
    type Small: Clone + Iso<Small = Self::Small, Big = Self::Big>;
    type Big: Clone + Iso<Small = Self::Small, Big = Self::Big>;

    fn small(&self) -> Self::Small;
    fn big(&self) -> Self::Big;
}

pub type Ambi<A, B> = std::sync::Arc<dyn Iso<Small = A, Big = B>>;

#[macro_export]
macro_rules! isotest {
    ($a:ty => $forward:expr, $b:ty => $backward:expr $(,)?) => {
        impl $crate::Iso for $a {
            type Small = $a;
            type Big = $b;

            fn small(&self) -> Self::Small {
                self.clone()
            }

            fn big(&self) -> Self::Big {
                let f: Box<dyn Fn($a) -> $b> = Box::new($forward);
                f(self.clone())
            }
        }

        impl $crate::Iso for $b {
            type Small = $a;
            type Big = $b;

            fn small(&self) -> Self::Small {
                let f: Box<dyn Fn($b) -> $a> = Box::new($backward);
                f(self.clone())
            }

            fn big(&self) -> Self::Big {
                self.clone()
            }
        }
    };
}

// pub fn unbox_small<A, B>(iso: Ambi<A, B>) -> A
// where
//     A: Iso<Small = A, Big = B> + 'static,
//     B: Iso<Small = A, Big = B> + 'static,
// {
//     let any: &dyn Any = &iso;
//     if let Some(a) = any.downcast_ref::<Arc<A>>() {
//         a.small()
//     } else if let Some(b) = any.downcast_ref::<Arc<B>>() {
//         b.small()
//     } else {
//         panic!("item is not of either expected Ambi type")
//     }
// }

fn generate1<A, B>(x: A) -> Ambi<A, B>
where
    A: Iso<Small = A, Big = B> + 'static,
    B: Iso<Small = A, Big = B> + 'static,
{
    Arc::new(x)
}

fn generate2<A, B>(x: A) -> Ambi<A, B>
where
    A: Iso<Small = A, Big = B> + 'static,
    B: Iso<Small = A, Big = B> + 'static,
{
    Arc::new(x.big())
}

fn modify1<A, B, M>(x: Ambi<A, B>, f: M) -> Ambi<A, B>
where
    A: Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
    M: Fn(A) -> A,
{
    Arc::new(f(x.small()))
}

fn modify2<A, B, M>(x: Ambi<A, B>, f: M) -> Ambi<A, B>
where
    A: Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
    M: Fn(A) -> A,
{
    Arc::new(f(x.small()).big())
}

type Create<A, B> = Box<dyn Fn(A) -> Ambi<A, B>>;
type Update<A, B, M> = Box<dyn Fn(Ambi<A, B>, M) -> Ambi<A, B>>;

pub fn run<A, B, M>(runner: impl Fn(Create<A, B>, Update<A, B, M>))
where
    A: Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
    M: Fn(A) -> A + 'static,
{
    runner(Box::new(generate1), Box::new(modify1));
    runner(Box::new(generate2), Box::new(modify2));
}
