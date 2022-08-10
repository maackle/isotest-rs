use std::sync::Arc;

pub trait Isotest: Sized + Runners<Self> {
    type Super: Runners<Self>;

    fn expand(&self) -> Self::Super;
    fn condense(big: Self::Super) -> Self;
}

// fn modify1<Iso: Isotest>(r: Gen<Iso>, f: Mutation<Iso>) -> Gen<Iso> {
//     Box::new(f(*r))
// }

pub trait Runners<Iso: Isotest> {
    fn as_iso(self: Arc<Self>) -> Iso {
        todo!()
    }

    // // fn generate1(&self) -> Box<Iso> {
    // //     Box::new(self.clone())
    // // }

    // fn modify1(self: Arc<Self>, f: Mutation<Iso>) -> Box<Iso> {
    //     // Self::generate1(f(*self))
    //     Box::new(f(self.as_iso()))
    // }

    // // fn generate2(self) -> Box<Iso::Super> {
    // //     Box::new(self.expand())
    // // }

    // fn modify2(self: Arc<Self>, f: Mutation<Iso>) -> Box<Iso::Super> {
    //     Box::new(f(self.as_iso()).expand())
    // }
}

fn generate1<Iso: Isotest + 'static>(x: Iso) -> Gen<Iso> {
    Arc::new(x)
}

fn modify1<Iso: Isotest + 'static>(x: Gen<Iso>, f: Mutation<Iso>) -> Gen<Iso> {
    // Self::generate1(f(*self))
    Arc::new(f(x.as_iso()))
}

fn modify2<Iso: Isotest + 'static>(x: Gen<Iso>, f: Mutation<Iso>) -> Gen<Iso> {
    // Self::generate1(f(*self))
    Arc::new(f(x.as_iso()).expand())
}

type Mutation<T> = Box<dyn Fn(T) -> T>;

type Gen<T> = Arc<dyn Runners<T>>;
type Create<T> = Box<dyn Fn(T) -> Gen<T>>;
type Update<T> = Box<dyn Fn(Gen<T>, Mutation<T>) -> Arc<dyn Runners<T>>>;
// type Create<T: Isotest> = Box<dyn Fn(T) -> Gen<T>>;
// type Update<T: Isotest> = Box<dyn Fn(Gen<T>, Mutation<T>) -> Gen<T>>;

// enum Closure<T> {
//     Create(Create<T>),
//     Update(Update<T>),
// }

// impl<A: Isotest> Closure<A> {
//     fn expand(&self) -> Closure<A::Super> {
//         match self {
//             Self::Create(f) => Closure::Create(Box::new(|| f().expand())),
//             Self::Update(f) => Closure::Update(Box::new(|a, f| f(A::condense(a)).expand())),
//         }
//     }
// }

pub fn run<I: Isotest + 'static>(
    runner: impl Fn(Create<I>, Update<I>),
    // runner2: impl Fn(Create<T>, Update<T>),
) {
    runner(Box::new(generate1), Box::new(modify1));
    // runner2(Box::new(generate2), Box::new(modify2));
}
