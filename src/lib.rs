use std::sync::Arc;

/// The container type used
pub type Bag<T> = Arc<T>;

pub trait Iso: 'static {
    type Small: Clone + Iso<Small = Self::Small, Big = Self::Big>;
    type Big: Clone + Iso<Small = Self::Small, Big = Self::Big>;

    fn small(&self) -> Self::Small;
    fn big(&self) -> Self::Big;
}

#[derive(Clone)]
pub struct Ambi<A, B>(Bag<dyn Iso<Small = A, Big = B>>);

impl<A, B> Ambi<A, B> {
    pub fn new(x: impl Iso<Small = A, Big = B>) -> Self {
        Self(Bag::new(x))
    }
}

impl<A, B> Iso for Ambi<A, B>
where
    A: Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
{
    type Small = A;
    type Big = B;

    fn small(&self) -> Self::Small {
        self.0.small()
    }

    fn big(&self) -> Self::Big {
        self.0.big()
    }
}

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

fn generate1<A, B>(x: A) -> Ambi<A, B>
where
    A: Iso<Small = A, Big = B> + 'static,
    B: Iso<Small = A, Big = B> + 'static,
{
    Ambi::new(x)
}

fn generate2<A, B>(x: A) -> Ambi<A, B>
where
    A: Iso<Small = A, Big = B> + 'static,
    B: Iso<Small = A, Big = B> + 'static,
{
    Ambi::new(x.big())
}

fn modify1<A, B, M>(x: Ambi<A, B>, f: M) -> Ambi<A, B>
where
    A: Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
    M: Fn(A) -> A,
{
    Ambi::new(f(x.small()))
}

fn modify2<A, B, M>(x: Ambi<A, B>, f: M) -> Ambi<A, B>
where
    A: Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
    M: Fn(A) -> A,
{
    Ambi::new(f(x.small()).big())
}

type Create<A, B> = Box<dyn Fn(A) -> Ambi<A, B>>;
type Update<A, B, M> = Box<dyn Fn(Ambi<A, B>, M) -> Ambi<A, B>>;

// pub fn run<A, B, M, C1, C2, U1, U2>(runner1: impl Fn(C1, U1), runner2: impl Fn(C2, U2))
pub fn run<A, B, M>(
    runner: impl Fn(Create<A, B>, Update<A, B, M>),
    // runner2: impl Fn(Create<A, B>, Update<A, B, M>),
) where
    A: Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
    M: Fn(A) -> A + 'static,
    // C1: Fn(A) -> A,
    // C2: Fn(A) -> B,
    // U1: Fn(A, M) -> A,
    // U2: Fn(B, M) -> B,
{
    runner(Box::new(generate1), Box::new(modify1));
    runner(Box::new(generate2), Box::new(modify2));
    // runner1(Box::new(|x: A| x.small()), Box::new(|x, f| f(x.small())));
    // runner2(Box::new(|x| x.big()), Box::new(|x, f| f(x.small()).big()));
}

impl<A, B> PartialEq for Ambi<A, B>
where
    A: PartialEq + Clone + Iso<Small = A, Big = B> + 'static,
    B: PartialEq + Clone + Iso<Small = A, Big = B> + 'static,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.small().eq(&other.0.small())
    }
}

impl<A, B> PartialOrd for Ambi<A, B>
where
    A: PartialOrd + Clone + Iso<Small = A, Big = B> + 'static,
    B: PartialOrd + Clone + Iso<Small = A, Big = B> + 'static,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.small().partial_cmp(&other.0.small())
    }
}

impl<A, B> std::fmt::Debug for Ambi<A, B>
where
    A: std::fmt::Debug + Clone + Iso<Small = A, Big = B> + 'static,
    B: Clone + Iso<Small = A, Big = B> + 'static,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Ambi").field(&self.0.small()).finish()
    }
}
