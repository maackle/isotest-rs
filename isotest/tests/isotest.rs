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
    isotest::isotest!(<TestStruct, RealStruct> |create, update, _| {
        let x = create(TestStruct(1));
        let y = create(TestStruct(2));
        let z = create(TestStruct(3));
        assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 6);

        let y = update(y, Box::new(|mut y: TestStruct| {
            y.0 = 4;
            y
        }));
        assert_eq!(process([x, y, z].into_iter()), 8);
    });
}

/// Test that an incorrect
#[test]
fn big_fails() {
    isotest::isotest!(
        < TestStruct, BadStruct > |create, update, ctx| {
            let x = create(TestStruct(1));
            let y = create(TestStruct(2));
            let z = create(TestStruct(3));

            match ctx {
                IsotestContext::Test => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 6),
                IsotestContext::Real => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 0),
            }

            let y = update(
                y,
                Box::new(|mut y: TestStruct| {
                    y.0 = 4;
                    y
                }),
            );

            match ctx {
                IsotestContext::Test => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 8),
                IsotestContext::Real => assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 0),
            }
        }
    );
}
