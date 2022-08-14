use isotest::IsotestContext;

trait Common {
    fn num(&self) -> u8;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, derive_more::Add, derive_more::Sum)]
struct TestStruct(u8);

#[derive(Copy, Clone, Debug, PartialEq, Eq, derive_more::Add, derive_more::Sum)]
struct RealStruct(u8, u8);
#[derive(Copy, Clone, Debug, PartialEq, Eq, derive_more::Add, derive_more::Sum)]
struct BadStruct(u8, u8);

isotest::iso! {
    TestStruct => |a| { RealStruct(a.0, 0) },
    RealStruct => |b| { TestStruct(b.0) },
}

isotest::iso! {
    TestStruct => |a| { BadStruct(a.0, 0) },
    BadStruct => |b| { TestStruct(b.0) },
}

impl Common for TestStruct {
    fn num(&self) -> u8 {
        self.0
    }
}

impl Common for RealStruct {
    fn num(&self) -> u8 {
        self.0
    }
}

// This is a faulty implementation!
impl Common for BadStruct {
    fn num(&self) -> u8 {
        self.0 * self.1
    }
}

fn process<T: Common, I: Iterator<Item = T>>(ts: I) -> u8 {
    ts.map(|t| Common::num(&t)).sum()
}

#[test]
fn basic() {
    isotest::isotest!(<TestStruct, RealStruct> |iso| {
        let x = iso.create(TestStruct(1));
        let y = iso.create(TestStruct(2));
        let mut z = iso.create(TestStruct(3));
        assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 6);

        let y = iso.update(y, |mut y| {
            y.0 = 4;
            y
        });
        assert_eq!(process([x, y, z].into_iter()), 8);

        iso.mutate(&mut z, |z| {
            z.0 = 10;
        });
        assert_eq!(process([x, y, z].into_iter()), 15);
    });
}

/// Test that an incorrect
#[test]
fn big_fails() {
    isotest::isotest!(
        < TestStruct, BadStruct > |iso| {
            let x = iso.create(TestStruct(1));
            let y = iso.create(TestStruct(2));
            let z = iso.create(TestStruct(3));

            match iso.context() {
                IsotestContext::Test => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 6),
                IsotestContext::Real => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 0),
            }

            let y = iso.update(
                y,
                |mut y| {
                    y.0 = 4;
                    y
                },
            );

            match iso.context() {
                IsotestContext::Test => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 8),
                IsotestContext::Real => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 0),
            }
        }
    );
}
