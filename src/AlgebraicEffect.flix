mod AlgebraicEffect {
    // RPN expressions as an algebraic effect
    pub eff IntRPN {
        def num(n: Int32): Unit
        def add(): Unit
        def sub(): Unit
        def mul(): Unit
    }

    // a pretty printer for RPN expressions
    pub def runPretty(expr: Unit -> Unit \ ef): String \ ef - IntRPN =
        region rc {
            let buffer: MutList[String, rc] = MutList.empty(rc);
            run {
                expr();
                MutList.join(" ", buffer)
            } with handler IntRPN {
                def num(n, resume) = {
                    MutList.push("${n}", buffer);
                    resume()
                }
                def add(_, resume) = {
                    MutList.push("+", buffer);
                    resume()
                }
                def sub(_, resume) = {
                    MutList.push("-", buffer);
                    resume()
                }
                def mul(_, resume) = {
                    MutList.push("*", buffer);
                    resume()
                }
            }
        }

    // an evaluator for RPN expressions
    pub def runEval(expr: Unit -> Unit \ ef): Result[String, Int32] \ ef - IntRPN =
        region rc {
            let stack: MutList[Int32, rc] = MutList.empty(rc);
            run {
                expr();
                match MutList.pop(stack) {
                    case None => Err("Empty stack")
                    case Some(result) => Ok(result)
                }
            } with handler IntRPN {
                def num(n, resume) = {
                    MutList.push(n, stack);
                    resume()
                }
                def add(_, resume) = match (MutList.pop(stack), MutList.pop(stack)) {
                    case (Some(n1), Some(n2)) =>
                        MutList.push(n2 + n1, stack);
                        resume()
                    case _ => Err("Not enough operands for addition")
                }
                def sub(_, resume) = match (MutList.pop(stack), MutList.pop(stack)) {
                    case (Some(n1), Some(n2)) =>
                        MutList.push(n2 - n1, stack);
                        resume()
                    case _ => Err("Not enough operands for subtraction")
                }
                def mul(_, resume) = match (MutList.pop(stack), MutList.pop(stack)) {
                    case (Some(n1), Some(n2)) =>
                        MutList.push(n2 * n1, stack);
                        resume()
                    case _ => Err("Not enough operands for multiplication")
                }
            }
        }

    // an example expression: 1 2 + 3 * 4 -

    def exp(): Unit \ IntRPN = {
        IntRPN.num(1);
        IntRPN.num(2);
        IntRPN.add();
        IntRPN.num(3);
        IntRPN.mul();
        IntRPN.num(4);
        IntRPN.sub()
    }

    pub def example(): Unit \ IO =
        let s = run { exp() } with runPretty;
        println("exp pritter prints to: ${s}");
        let n = run { exp() } with runEval;
        println("exp evaluates to: ${n}")
}
