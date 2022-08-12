trait Common {
    fn num(&self) -> u8;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, derive_more::Add, derive_more::Sum)]
struct TestStruct(u8);

#[derive(Copy, Clone, Debug, PartialEq, Eq, derive_more::Add, derive_more::Sum)]
struct RealStruct(u8, u8);

isotest::isotest! {
    TestStruct => |a| { RealStruct(a.0, 0) },
    RealStruct => |b| { TestStruct(b.0) },
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

impl Common for isotest::Ambi<TestStruct, RealStruct> {
    fn num(&self) -> u8 {
        self.small().num()
    }
}

fn process<T: Common, I: Iterator<Item = T>>(ts: I) -> u8 {
    ts.map(|t| Common::num(&t)).sum()
}

#[test]
fn basic() {
    isotest::run(|create, update| {
        let x = create(TestStruct(1));
        let y = create(TestStruct(2));
        let z = create(TestStruct(3));
        assert_eq!(process([x.clone(), y.clone(), z.clone()].into_iter()), 6);

        let y = update(y, |mut y: TestStruct| {
            y.0 = 4;
            y
        });
        assert_eq!(process([x, y, z].into_iter()), 8);
    });
}
