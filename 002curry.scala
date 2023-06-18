object Func {
    
    def curry[A,B,C](f: (A, B) => C): A => B => C =
        a => b => f(a, b)
    
    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
        (a, b) => f(a)(b)
    
    def compose[A,B,C](f: A => B, g: B => C): A => C =
        a => g(f(a))

    def add(a: Int, b: Int): Int = 
        a + b
}

object Main extends App {
    val plus = Func.curry(Func.add)
    val plus2 = Func.uncurry(plus)
    println(
        plus2(1, 2)
    )
}


