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
pub trait Iso: Sized {
    /// The real data which corresponds to the type this trait is defined for
    type Real: Clone + From<Self>;

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
    (
        $a:ty => $forward:expr,
        $b:ty => $backward:expr
        $(, test_cases: [$($tc:expr),* $(,)?])?
        $(, real_cases: [$($rc:expr),* $(,)?])?
        $(,)?
    ) => {
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

        $( paste::paste! {
            #[test]
            fn [< iso_impl_invariants_test__ $a:snake:lower __ $b:snake:lower >]() {
                $(
                    let test1: $a = $tc;
                    let test2 = $crate::roundtrip_test(test1);
                    assert_eq!(test1, test2, "Iso test_case invariant test failed: {:?} != {:?}", test1, test2);
                )*
            }
        })?

        $(paste::paste! {
            #[test]
            fn [< iso_impl_invariants_real__ $a:snake:lower __ $b:snake:lower >]() {
                $(
                    let real: $b = $rc;
                    let (test1, test2) = $crate::roundtrip_real::<$a, $b>(real);
                    assert_eq!(test1, test2, "Iso real_case invariant test failed: {:?} != {:?}, real data = {:?}", test1, test2, real);
                )*
            }
        })?
    };
}

/// Roundtrip from test -> real -> test
pub fn roundtrip_test<A>(test: A) -> A
where
    A: Iso + PartialEq + std::fmt::Debug,
{
    A::test(&test.real())
}

/// Roundtrip from real -> test -> real -> test, returning the two test items
pub fn roundtrip_real<A, B>(real: B) -> (A, A)
where
    A: Iso<Real = B> + PartialEq + std::fmt::Debug,
    B: Clone + PartialEq + std::fmt::Debug,
{
    let test = A::test(&real);
    let test2 = A::test(&test.real());
    (test, test2)
}

/// Test the invariants of your Iso implementation.
/// This test must pass for any Test value you use.
pub fn assert_iso_invariants_test<A>(test: A)
where
    A: Iso + PartialEq + std::fmt::Debug,
{
    let test2 = A::test(&test.real());
    assert_eq!(
        test, test2,
        "test -> real -> test roundtrip should leave original value unchanged"
    );
}

/// Test the invariants of your Iso implementation.
/// This test must pass for any Real value you use.
pub fn assert_iso_invariants_real<A, B>(real: B)
where
    A: Iso<Real = B> + PartialEq + std::fmt::Debug,
    B: Clone + PartialEq + std::fmt::Debug,
{
    {
        let test = A::test(&real);
        let test2 = A::test(&test.real());
        assert_eq!(
            test, test2,
            "real -> test -> real -> test roundtrip should be idempotent"
        );
    }
}

/// Run the same closure for both the Test and Real versions of some data.
///
/// The macro takes a list of [`Iso`] implementors, followed by closure which
/// receives a small API which can handle each `Iso` type. See
/// [`IsoTestApi`] and [`IsoRealApi`] for descriptions of the methods.
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
/// };
///
/// isotest::isotest!(A => |iso| {
///     let mut a = iso.create(A(1));
///     assert_eq!(a.num(), 1);
///     iso.mutate(&mut a, |a| {
///         a.0 = 2;
///     });
///     assert_eq!(a.num(), 2);
/// });
/// ```
///
#[macro_export]
macro_rules! isotest {
    ( $($iso:ty),+ => $runner:expr) => {
        use $crate::Iso;
        {
            // This is the test using the "test" struct
            let run: Box<dyn Fn($( $crate::IsoTestApi<$iso>, )+ )> = Box::new($runner);
            run($( $crate::IsoTestApi::<$iso>::new(), )+);
        }
        {
            // This is the test using the "real" struct
            let run: Box<dyn Fn($( $crate::IsoRealApi<$iso>, )+ )> = Box::new($runner);
            run($( $crate::IsoRealApi::<$iso>::new(), )+);
        }
    };
}

/// A version of [`isotest!`] which returns `Future<Output = ()>` instead of `()`
/// and supports `async` syntax.
#[cfg(feature = "async")]
#[macro_export]
macro_rules! isotest_async {
    ($($iso:ty),+ => $runner:expr) => {
        use $crate::Iso;
        {
            // This is the test using the "test" struct
            let run: Box<dyn Fn($( $crate::IsoTestApi<$iso>, )+ )
                -> std::pin::Pin<Box<dyn futures::Future<Output = ()>>>,
            > = Box::new($runner);
            run($( $crate::IsoTestApi::<$iso>::new(), )+).await;
        }
        {
            // This is the test using the "real" struct
            let run: Box<dyn Fn($( $crate::IsoRealApi<$iso>, )+ )
                -> std::pin::Pin<Box<dyn futures::Future<Output = ()>>>,
            > = Box::new($runner);
            run($( $crate::IsoRealApi::<$iso>::new(), )+).await;
        }
    };
}

/// The API passed into an isotest in the Test context.
///
/// The `isotest!` macro is a bit sneaky, passing in APIs with different
/// function signatures for each context, so that the test can be written
/// the same lexically, but actually expand to two different tests
/// working with two different types.
pub struct IsoTestApi<A: Iso>(PhantomData<A>);

/// The API passed into an isotest in the Real context.
///
/// The `isotest!` macro is a bit sneaky, passing in APIs with different
/// function signatures for each context, so that the test can be written
/// the same lexically, but actually expand to two different tests
/// working with two different types.
pub struct IsoRealApi<A: Iso>(PhantomData<A>);

impl<A: Iso> IsoTestApi<A> {
    /// Constructor
    pub fn new() -> Self {
        Self(PhantomData)
    }

    /// Create test data from test data (identity function)
    pub fn create(&self, a: A) -> A {
        a
    }

    /// Update test data with a function over test data (simple map)
    pub fn update(&self, a: A, f: impl Fn(A) -> A) -> A {
        f(a)
    }

    /// Mutate test data with a function over test data (simple mutable map)
    pub fn mutate(&self, a: &mut A, f: impl Fn(&mut A)) {
        f(a)
    }

    /// Return the context we're in
    pub fn context(&self) -> IsotestContext {
        IsotestContext::Test
    }
}

impl<A: Iso> IsoRealApi<A> {
    /// Constructor
    pub fn new() -> Self {
        Self(PhantomData)
    }

    /// Create real data from test data
    pub fn create(&self, a: A) -> A::Real {
        a.real()
    }

    /// Update real data with a function over test data
    pub fn update(&self, x: A::Real, f: impl Fn(A) -> A) -> A::Real {
        f(A::test(&x)).real()
    }

    /// Mutate real data with a function over test data (simple mutable map)
    pub fn mutate(&self, x: &mut A::Real, f: impl Fn(&mut A)) {
        let mut t = A::test(x);
        f(&mut t);
        let _ = std::mem::replace(x, t.real());
    }

    /// Return the context we're in
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
