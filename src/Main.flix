use Sys.Process

// The main entry point.
def main(): Unit \ { Process, Abort, IO } =
    println("Hello World!");

    let n = 5;
    println("The square of ${n} is ${square(n)}.");

    runProcess()
    |> Result.getOrAbort
    |> x -> println("Process output:\n${x}")

def square(x: Int32): Int32 =
    x * x

def runProcess(): Result[IoError, String] \ { Process, IO } =
    region rc {
        forM (
            ph <- Process.exec("ls", Nil);
            out <- Process.stdout(ph);
            buf = Array.repeat(rc, 1024, 0i8);
            _ <- Readable.read(buf, out)
        ) yield buf |> Array.toVector |> String.fromBytes
    }
