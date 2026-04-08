mod FreeMonad {
    // a Free monad over a Functor f

    pub enum Free[f: Type -> Type, a: Type] {
        case Pure(a),
        case Free(f[Free[f, a]])
    }

    instance Functor[Free[f]] with Functor[f] {
        pub def map(f: a -> b \ ef, x: Free[f, a]): Free[f, b] \ ef = match x {
            case Free.Pure(a) => Free.Pure(f(a))
            case Free.Free(fa) =>
                Free.Free(Functor.map((a: Free[f, a]) -> Functor.map(f, a), fa))
        }
    }

    instance Applicative[Free[f]] with Functor[f] {
        pub def point(x: a): Free[f, a] =
            Free.Pure(x)

        pub def ap(f: Free[f, a -> b \ ef], x: Free[f, a]): Free[f, b] \ ef =
            Monad.flatMap(g -> Functor.map(g, x), f)
    }

    instance Monad[Free[f]] with Functor[f] {
        pub def flatMap(f: a -> Free[f, b] \ ef, x: Free[f, a]): Free[f, b] \ ef = match x {
            case Free.Pure(a) => f(a)
            case Free.Free(g) =>
                Free.Free(Functor.map((fa: Free[f, a]) -> Monad.flatMap(f, fa), g))
        }
    }

    pub def liftF(fa: f[a]): Free[f, a] with Functor[f] =
        Free.Free(Functor.map(a -> Free.Pure(a), fa))

    // RPN expressions as a Functor

    enum RPNExpr[n, next] {
        case Num(n, next),
        case Add(next),
        case Sub(next),
        case Mul(next)
    }

    instance Functor[RPNExpr[n]] {
        pub def map(f: a -> b \ ef, x: RPNExpr[n, a]): RPNExpr[n, b] \ ef = match x {
            case RPNExpr.Num(n, next) => RPNExpr.Num(n, f(next))
            case RPNExpr.Add(next) => RPNExpr.Add(f(next))
            case RPNExpr.Sub(next) => RPNExpr.Sub(f(next))
            case RPNExpr.Mul(next) => RPNExpr.Mul(f(next))
        }
    }

    // RPN expressions as a Free monad over RPNExpr

    pub type alias RPN[a, b] = Free[RPNExpr[a], b]

    pub def num(n: a): RPN[a, Unit] =
        liftF(RPNExpr.Num(n, ()))

    pub def add(): RPN[a, Unit] =
        liftF(RPNExpr.Add(()))

    pub def sub(): RPN[a, Unit] =
        liftF(RPNExpr.Sub(()))

    pub def mul(): RPN[a, Unit] =
        liftF(RPNExpr.Mul(()))

    // a pretty printer for RPN expressions
    pub def pretty(expr: RPN[a, b]): String with ToString[a] = match expr {
        case Free.Pure(_) => ""
        case Free.Free(RPNExpr.Num(n, next)) =>
            "${n} ${pretty(next)}"
        case Free.Free(RPNExpr.Add(next)) =>
            "+ ${pretty(next)}"
        case Free.Free(RPNExpr.Sub(next)) =>
            "- ${pretty(next)}"
        case Free.Free(RPNExpr.Mul(next)) =>
            "* ${pretty(next)}"
    }

    // an evaluator for RPN expressions
    pub def eval(expr: RPN[a, b]): Result[String, a] with Add[a], Sub[a], Mul[a] = {
        def calc(stack: List[a], e: RPN[a, b]): Result[String, a] = match (stack, e) {
            case (s, Free.Free(RPNExpr.Num(n, next))) =>
                calc(n :: s, next)
            case (n1 :: n2 :: ns, Free.Free(RPNExpr.Add(next))) =>
                calc(n2 + n1 :: ns, next)
            case (n1 :: n2 :: ns, Free.Free(RPNExpr.Sub(next))) =>
                calc(n2 - n1 :: ns, next)
            case (n1 :: n2 :: ns, Free.Free(RPNExpr.Mul(next))) =>
                calc(n2 * n1 :: ns, next)
            case (n :: _, Free.Pure(_)) =>
                Ok(n)
            case _ =>
                Err("Invalid expression")
        };
        calc(Nil, expr)
    }

    // an example expression: 1 2 + 3 * 4 -

    def exp1(): RPN[Int32, Unit] =
        forM (
            _ <- num(1);
            _ <- num(2);
            _ <- add();
            _ <- num(3);
            _ <- mul();
            _ <- num(4);
            _ <- sub()
        ) yield ()

    pub def example1(): Unit \ IO =
        println("exp pretty prints to: ${pretty(exp1())}");
        println("exp evaluates to: ${eval(exp1())}")

    // the same expression using applicative style
    def exp2(): RPN[Int32, Unit] =
        use Applicative.*>;
        num(1) *> num(2) *> add() *> num(3) *> mul() *> num(4) *> sub()

    pub def example2(): Unit \ IO =
        println("exp pretty prints to: ${pretty(exp2())}");
        println("exp evaluates to: ${eval(exp2())}")
}
