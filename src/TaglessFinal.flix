mod TaglessFinal {
    // expressions as a type class
    pub trait Expr[repr: Type -> Type] {
        pub def num(n: a): repr[a] with ToString[a]
        pub def bool(b: Bool): repr[Bool]
        pub def add(e1: repr[a], e2: repr[a]): repr[a] with Add[a]
        pub def mul(e1: repr[a], e2: repr[a]): repr[a] with Mul[a]
        pub def eq(e1: repr[a], e2: repr[a]): repr[Bool] with Eq[a]
    }

    // an evaluator

    enum Eval[a](a)

    pub def runEval(x: Eval[a]): a = match x {
        case Eval.Eval(value) => value
    }

    // an implementation of the expression language as an evaluator
    instance Expr[Eval] {
        pub def num(n: a): Eval[a] with ToString[a] = Eval.Eval(n)
        pub def bool(b: Bool): Eval[Bool] = Eval.Eval(b)
        pub def add(e1: Eval[a], e2: Eval[a]): Eval[a] with Add[a] = match (e1, e2) {
            case (Eval.Eval(x), Eval.Eval(y)) => Eval.Eval(x + y)
        }
        pub def mul(e1: Eval[a], e2: Eval[a]): Eval[a] with Mul[a] = match (e1, e2) {
            case (Eval.Eval(x), Eval.Eval(y)) => Eval.Eval(x * y)
        }
        pub def eq(e1: Eval[a], e2: Eval[a]): Eval[Bool] with Eq[a] = match (e1, e2) {
            case (Eval.Eval(x), Eval.Eval(y)) => Eval.Eval(x == y)
        }
    }

    // a pretty printer

    enum Pretty[_a](String)

    pub def runPretty(x: Pretty[a]): String = match x {
        case Pretty.Pretty(s) => s
    }

    // an implementation of the expression language as a pretty printer
    instance Expr[Pretty] {
        pub def num(n: a): Pretty[a] with ToString[a] = Pretty.Pretty("${n}")
        pub def bool(b: Bool): Pretty[Bool] = Pretty.Pretty("${b}")
        pub def add(e1: Pretty[a], e2: Pretty[a]): Pretty[a] with Add[a] = match (e1, e2) {
            case (Pretty.Pretty(s1), Pretty.Pretty(s2)) => Pretty.Pretty("(${s1} + ${s2})")
        }
        pub def mul(e1: Pretty[a], e2: Pretty[a]): Pretty[a] with Mul[a] = match (e1, e2) {
            case (Pretty.Pretty(s1), Pretty.Pretty(s2)) => Pretty.Pretty("(${s1} * ${s2})")
        }
        pub def eq(e1: Pretty[a], e2: Pretty[a]): Pretty[Bool] with Eq[a] = match (e1, e2) {
            case (Pretty.Pretty(s1), Pretty.Pretty(s2)) => Pretty.Pretty("(${s1} == ${s2})")
        }
    }

    // examples of using the expression language with both interpreters

    pub def evalExample1(): Unit \ IO =
        use Expr.{num, add, mul, eq};
        let e1 = add(num(1), mul(num(2), num(3)));
        let e2 = eq(e1, num(7));
        println("e1 evaluates to ${runEval(e1)}");
        println("e2 evaluates to ${runEval(e2)}")

    pub def prettyExample1(): Unit \ IO =
        use Expr.{num, add, mul, eq};
        let e1 = add(num(1), mul(num(2), num(3)));
        let e2 = eq(e1, num(7));
        println("e1 pretty prints to ${runPretty(e1)}");
        println("e2 pretty prints to ${runPretty(e2)}")

    // using generic functions to avoid repeating the same code for both interpreters

    def exp1(): repr[Int32] with Expr[repr] =
        use Expr.{num, add, mul};
        add(num(1), mul(num(2), num(3)))

    def exp2(): repr[Bool] with Expr[repr] =
        use Expr.{num, eq};
        eq(exp1(), num(7))

    pub def evalExample2(): Unit \ IO =
        println("e1 evaluates to ${runEval(exp1())}");
        println("e2 evaluates to ${runEval(exp2())}")

    pub def prettyExample2(): Unit \ IO =
        println("e1 pretty prints to ${runPretty(exp1())}");
        println("e2 pretty prints to ${runPretty(exp2())}")
}
