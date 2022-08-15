//! Isotest enables a very specific Rust unit testing pattern.

#![warn(missing_docs)]

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
pub trait Iso {
    /// The real data which corresponds to the type this trait is defined for
    type Real: Clone;

    /// Return the test version of this data, mapping if necessary.
    ///
    /// The test data must be a subset of the real data, so that this
    /// transformation is never lossy.
    fn test(x: &Self::Real) -> Self;

    /// Return the real version of this data, mapping if necessary.
    ///
    /// In general, the real data is a superset of the test data,
    /// so some arbitrary data will need to be supplied to fill in
    /// the rest. In fact, it is best if truly random arbitrary
    /// data is used, as this will act as a fuzz test of your
    /// trait implementation.
    fn real(&self) -> Self::Real;
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
/// assert_eq!(A(1).real(), B(1, 42));
/// assert_eq!(A::test(&B(1, 2)), A(1));
/// ```
#[macro_export]
macro_rules! iso {
    ($a:ty => $forward:expr, $b:ty => $backward:expr $(,)?) => {
        impl $crate::Iso for $a {
            type Real = $b;

            fn test(x: &$b) -> $a {
                let f: Box<dyn Fn($b) -> $a> = Box::new($backward);
                f(x.clone())
            }

            fn real(&self) -> $b {
                let f: Box<dyn Fn($a) -> $b> = Box::new($forward);
                f(self.clone())
            }
        }

        impl From<$a> for $b {
            fn from(a: $a) -> $b {
                use $crate::Iso;
                a.real()
            }
        }
    };
}

/// Test the invariants of your Iso implementation with an arbitrary Test and Real value.
///
/// The Test and Real values do not need to have any relationship to each other.
pub fn test_iso_invariants<A, B>(test: A, real: B)
where
    A: Iso<Real = B> + PartialEq + std::fmt::Debug,
    B: Clone + PartialEq + std::fmt::Debug,
{
    {
        let test2 = A::test(&test.real());
        assert_eq!(
            test, test2,
            "test -> real -> test roundtrip should leave original value unchanged"
        );
    }
    {
        let test = A::test(&real);
        let test2 = A::test(&test.real());
        assert_eq!(
            test, test2,
            "real -> test -> real -> test roundtrip should be idempotent"
        );
    }
}

///
/// ```rust
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
///     |iso| {
///         let mut a = iso.create(A(1));
///         assert_eq!(a.num(), 1);
///         iso.mutate::<A, _>(&mut a, |a| {
///             a.0 = 2;
///         });
///         assert_eq!(a.num(), 2);
///     }
/// }
/// ```
#[macro_export]
macro_rules! isotest {
    ($runner:expr) => {
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

    pub fn update<A: Iso, F: Fn(A) -> A>(&self, a: A, f: F) -> A {
        f(a)
    }

    pub fn mutate<A, F>(&self, a: &mut A, f: F)
    where
        F: Fn(&mut A),
    {
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
        A: Iso<Real = B>,
        B: Clone,
    {
        a.real()
    }

    pub fn update<A, F: Fn(A) -> A>(&self, x: A::Real, f: F) -> A::Real
    where
        A: Iso,
    {
        f(A::test(&x)).real()
    }

    pub fn mutate<A, F>(&self, x: &mut A::Real, f: F)
    where
        A: Iso,
        F: Fn(&mut A),
    {
        let mut t = A::test(x);
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
