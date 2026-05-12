// ref. https://github.com/fpinscala/fpinscala/wiki/A-brief-introduction-to-Haskell,-and-why-it-matters
mod BriefIntroToHaskellExamples {
    use Sys.Console

    pub def main(): Unit \ {IO, Console} =
        println("Hello world!!");
        Console.println("Echo-inator v2.0");
        let s = Console.readln();
        Console.println("${s}")

    pub def f1(): Int32 = f2() + magicValue()

    pub def f2(): Int32 = 97
    def magicValue(): Int32 = 42

    pub def factorial(n: Int32): Int32 = match n {
        case 0 => 1
        case x => x * factorial(x - 1)
    }

    pub def factorial2(n: Int32): Int32 =
        @Tailrec
        def go(x: Int32, acc: Int32): Int32 = match x {
            case 0 => acc
            case y => go (y - 1, y * acc)
        };
        go(n, 1)

    pub enum List[a] {
        case Nil
        case Cons(a, List[a])
    }

    pub enum Option[a] {
        case None
        case Some(a)
    }

    pub def headOption(xs: List[a]): Option[a] = match xs {
        case List.Nil => Option.None
        case List.Cons(h, _) => Option.Some(h)
    }

    pub enum List2[a] {
        case Nil
        case Cons({head = a, tail = List2[a]})
    }

    pub def foldRight(f: (a, b) -> b, z: b, xs: List[a]): b = match xs {
        case List.Nil => z
        case List.Cons(h, t) => f(h, foldRight(f, z, t))
    }

    pub def takeWhile(f: a -> Bool, xs: List[a]): List[a] =
        foldRight((h, t) -> if (f(h)) List.Cons(h, t) else List.Nil, List.Nil, xs)

    pub trait Functor[f: Type -> Type] {
        pub def fmap(f: a -> b, fa: f[a]): f[b]
    }

    instance Functor[Option] {
        pub def fmap(f: a -> b, fa: Option[a]): Option[b] = match fa {
            case Option.None => Option.None
            case Option.Some(a) => Option.Some(f(a))
        }
    }

    pub def strength(x: a, fb: f[b]): f[(a, b)] with Functor[f] =
        Functor.fmap(b -> (x, b), fb)

    pub trait Monoid[a] {
        pub def mempty(): a
        pub def mappend(m1: a, m2: a): a
    }

    pub enum Sum(Int32)
    pub enum Product(Int32)

    instance Monoid[Sum] {
        pub def mempty(): Sum = Sum.Sum(0)
        pub def mappend(s1: Sum, s2: Sum): Sum =
            let Sum.Sum(n1) = s1;
            let Sum.Sum(n2) = s2;
            Sum.Sum(n1 + n2)
    }

    instance Monoid[Product] {
        pub def mempty(): Product = Product.Product(1)
        pub def mappend(p1: Product, p2: Product): Product =
            let Product.Product(n1) = p1;
            let Product.Product(n2) = p2;
            Product.Product(n1 * n2)
    }

    pub trait Monad[f: Type -> Type] {
        pub def return(a: a): f[a]
        pub def >>=(ma: f[a], f: a -> f[b]): f[b]
    }

    instance Monad[Option] {
        pub def return(a: a): Option[a] = Option.Some(a)
        pub def >>=(ma: Option[a], f: a -> Option[b]): Option[b] = match ma {
            case Option.None => Option.None
            case Option.Some(a) => f(a)
        }
    }

    pub enum State[s, a](s -> (a, s))

    instance Monad[State[s]] {
        pub def return(a: a): State[s, a] = State.State(s -> (a, s))
        pub def >>=(ma: State[s, a], f: a -> State[s, b]): State[s, b] =
            State.State(s0 ->
                let State.State(runState1) = ma;
                let (a, s1) = runState1(s0);
                let State.State(runState2) = f(a);
                runState2(s1)
            )
    }
}
