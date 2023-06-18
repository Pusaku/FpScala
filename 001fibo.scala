object fibo {
    def fibo(n: Int): Int = {
        if (n == 0) 0
        else if (n == 1) 1
        else fibo(n - 2) + fibo(n - 1)
    }

    def fibo2(n: Int): Int = {
        def go(n: Int, n1: Int, n2: Int): Int = {
            if (n == 0) n1
            else if (n == 1) n2
            else go(n - 1, n2, n1 + n2)
        }
        go(n, 0, 1)
    }
}

object Main extends App {
    println(fibo.fibo(25))
    println(fibo.fibo2(25))
}

