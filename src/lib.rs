//! Isotest enables a very specific Rust unit testing pattern.

#![warn(missing_docs)]

use std::marker::PhantomData;

/// Which of the two contexts each isotest body will run under.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IsotestContext {
    /// The "test" context, using a minimal test struct
    Test,
    /// The "real" context, using the actual struct used in the rest of the code
    Real,
}

/// Trait that declares the relationship between a "test" and "real struct",
/// namely how to go back and forth between the two.
pub trait Iso<Test, Real>: 'static
where
    Test: Clone + Iso<Test, Real>,
    Real: Clone + Iso<Test, Real>,
{
    /// Return the test version of this data, mapping if necessary.
    ///
    /// The test data must be a subset of the real data, so that this
    /// transformation is never lossy.
    fn test(&self) -> Test;

    /// Return the real version of this data, mapping if necessary.
    ///
    /// In general, the real data is a superset of the test data,
    /// so some arbitrary data will need to be supplied to fill in
    /// the rest. In fact, it is best if truly random arbitrary
    /// data is used, as this will act as a fuzz test of your
    /// trait implementation.
    fn real(&self) -> Real;
}

/// Helper to define a two-way [`Iso`] relationship between test data
/// and real data.
///
/// You typically do not need to work with this trait directly. However,
/// It must be implemented for the two types that you use in
/// an [`isotest!`] invocation.
///
/// ```rust
/// use isotest::Iso;
///
/// #[derive(Clone, Debug, PartialEq)]
/// struct A(u8);
/// #[derive(Clone, Debug, PartialEq)]
/// struct B(u8, u8);
///
/// isotest::iso! {
///     A => |a| B(a.0, 42),
///     B => |b| A(b.0),
/// }
///
/// assert_eq!(A(1).test(), A(1));
/// assert_eq!(A(1).real(), B(1, 42));
/// assert_eq!(B(1, 2).test(), A(1));
/// assert_eq!(B(1, 2).real(), B(1, 2));
/// ```
#[macro_export]
macro_rules! iso {
    ($a:ty => $forward:expr, $b:ty => $backward:expr $(,)?) => {
        impl $crate::Iso<$a, $b> for $a {
            fn test(&self) -> $a {
                self.clone()
            }

            fn real(&self) -> $b {
                let f: Box<dyn Fn($a) -> $b> = Box::new($forward);
                f(self.clone())
            }
        }

        impl $crate::Iso<$a, $b> for $b {
            fn test(&self) -> $a {
                let f: Box<dyn Fn($b) -> $a> = Box::new($backward);
                f(self.clone())
            }

            fn real(&self) -> $b {
                self.clone()
            }
        }
    };
}

///
/// ```rust
/// use isotest::Iso;
///
/// #[derive(Clone, Debug, PartialEq)]
/// struct A(u8);
/// #[derive(Clone, Debug, PartialEq)]
/// struct B(u8, u8);
///
/// trait X {
///     fn num(&self) -> u8;
/// }
///
/// impl X for A {
///     fn num(&self) -> u8 {
///         self.0
///     }
/// }
///
/// impl X for B {
///     fn num(&self) -> u8 {
///         self.0
///     }
/// }
///
/// isotest::iso! {
///     A => |a| B(a.0, 42),
///     B => |b| A(b.0),
/// }
///
/// isotest::isotest! {
///     <A, B>
///     |create, update, _| {
///         let a = create(A(1));
///         assert_eq!(a.num(), 1);
///         let a = update(a, Box::new(|mut a| {
///             a.0 = 2;
///             a
///         }));
///         assert_eq!(a.num(), 2);
///     }
/// }
/// ```
#[macro_export]
macro_rules! isotest {
    (< $test:ty , $real:ty > $runner:expr) => {
        use $crate::Iso;
        {
            // This is the test using the "test" struct
            let api = $crate::IsoTestApi;
            let run: Box<dyn Fn($crate::IsoTestApi)> = Box::new($runner);
            run(api);
        }
        {
            // This is the test using the "real" struct
            let api = $crate::IsoRealApi;
            let run: Box<dyn Fn($crate::IsoRealApi)> = Box::new($runner);
            run(api);
        }
    };
}

pub struct IsoTestApi;

impl IsoTestApi {
    pub fn create<A>(&self, a: A) -> A {
        a
    }

    pub fn update<A>(&self, a: A, f: impl Fn(A) -> A) -> A {
        f(a)
    }

    pub fn mutate<A>(&self, a: &mut A, f: impl Fn(&mut A)) {
        f(a)
    }

    pub fn context(&self) -> IsotestContext {
        IsotestContext::Test
    }
}

pub struct IsoRealApi;

impl IsoRealApi {
    pub fn create<A, B>(&self, a: A) -> B
    where
        A: Clone + Iso<A, B>,
        B: Clone + Iso<A, B>,
    {
        a.real()
    }

    pub fn update<A, B>(&self, x: B, f: impl Fn(A) -> A) -> B
    where
        A: Clone + Iso<A, B>,
        B: Clone + Iso<A, B>,
    {
        f(x.test()).real()
    }

    pub fn mutate<A, B>(&self, x: &mut B, f: impl Fn(&mut A))
    where
        A: Clone + Iso<A, B>,
        B: Clone + Iso<A, B>,
    {
        let mut t = x.test();
        f(&mut t);
        std::mem::replace(x, t.real());
    }

    pub fn context(&self) -> IsotestContext {
        IsotestContext::Real
    }
}

/// The argument to an isotest `update` function
pub type Modify<A> = Box<dyn Fn(A) -> A>;

/// Creation function for the test context
pub type CreateTest<A> = Box<dyn Fn(A) -> A>;
/// Update function for the test context
pub type UpdateTest<A> = Box<dyn Fn(A, Modify<A>) -> A>;
/// Create function for the real context
pub type CreateReal<A, B> = Box<dyn Fn(A) -> B>;
/// Update function for the real context
pub type UpdateReal<A, B> = Box<dyn Fn(B, Modify<A>) -> B>;
