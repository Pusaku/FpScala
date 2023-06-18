object Main extends  App {

    def hasSubSeq[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case Nil => false
        case x :: xs => {
            if (sup.take(sub.length) == sub) true
            else hasSubSeq(xs, sub) 
        }
    }

    println(
        hasSubSeq(List(1,2,3,4,5), List(2,3,5))
    )
}