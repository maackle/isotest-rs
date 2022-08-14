use std::sync::Arc;

/// The container type used
pub type Bag<T> = Arc<T>;

pub trait Iso<Small, Big>: 'static
where
    Small: Clone + Iso<Small, Big>,
    Big: Clone + Iso<Small, Big>,
{
    fn small(&self) -> Small;
    fn big(&self) -> Big;
}

#[macro_export]
macro_rules! iso {
    ($a:ty => $forward:expr, $b:ty => $backward:expr $(,)?) => {
        impl $crate::Iso<$a, $b> for $a {
            fn small(&self) -> $a {
                self.clone()
            }

            fn big(&self) -> $b {
                let f: Box<dyn Fn($a) -> $b> = Box::new($forward);
                f(self.clone())
            }
        }

        impl $crate::Iso<$a, $b> for $b {
            fn small(&self) -> $a {
                let f: Box<dyn Fn($b) -> $a> = Box::new($backward);
                f(self.clone())
            }

            fn big(&self) -> $b {
                self.clone()
            }
        }
    };
}

pub type Modify<A> = Box<dyn Fn(A) -> A>;
pub type Create1<A> = Box<dyn Fn(A) -> A>;
pub type Update1<A> = Box<dyn Fn(A, Modify<A>) -> A>;
pub type Create2<A, B> = Box<dyn Fn(A) -> B>;
pub type Update2<A, B> = Box<dyn Fn(B, Modify<A>) -> B>;

#[macro_export]
macro_rules! isotest {
    (< $small:ty , $big:ty > $runner:expr) => {
        // fn run1(runner: Box<dyn Fn(Create1<$small>, Update1<$small>)>) {
        //     todo!()
        // }
        // fn run1<M, C, U>(runner: impl Fn(C, U))
        // where
        //     // A: Clone + Iso<Small = A, Big = B> + 'static,
        //     // B: Clone + Iso<Small = A, Big = B> + 'static,
        //     M: Fn($small) -> $small,
        //     C: Fn($small) -> $small,
        //     U: Fn($small, M) -> $small,
        // {
        //     todo!()
        // }

        {
            let gen1: $crate::Create1<$small> = Box::new(|x: $small| x);
            let upd1: $crate::Update1<$small> =
                Box::new(|x: $small, f: $crate::Modify<$small>| f(x));
            let run: Box<dyn Fn($crate::Create1<$small>, $crate::Update1<$small>)> =
                Box::new($runner);
            run(gen1, upd1);
        }

        {
            let gen2: $crate::Create2<$small, $big> = Box::new(|x: $small| x.big());
            let upd2: $crate::Update2<$small, $big> =
                Box::new(|x: $big, f: $crate::Modify<$small>| f(x.small()).big());
            let run: Box<dyn Fn($crate::Create2<$small, $big>, $crate::Update2<$small, $big>)> =
                Box::new($runner);
            run(gen2, upd2);
        }
    };
}

// type Create<A, B> = Box<dyn Fn(A) -> Ambi<A, B>>;
// type Update<A, B, M> = Box<dyn Fn(Ambi<A, B>, M) -> Ambi<A, B>>;
