import scala.compiletime.ops.int
import scala.quoted.FromExpr.NilFromExpr
import scala.reflect.ManifestFactory.NothingManifest
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](haed: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(da: List[Double]): Double = da match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs) 
    }

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def reverse[A](as: List[A]): List[A] = 
        foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = 
        foldRight(a1, a2)((a, b) => Cons(a, b))

    def appendList[A](as: List[List[A]]): List[A] =
        foldRight(as, Nil: List[A])((a, b) => append(a, b))


    def length[A](as: List[A]): Int = 
        foldRight(as, 0)((_, b) => b + 1)


    def tail[A](la: List[A]): List[A] = la match {
        case Nil => Nil
        case Cons(x, xs) => xs
    }

    def drop[A](la: List[A], n: Int): List[A] = la match {
        case Nil => Nil
        case Cons(x, xs) => {
            if (n <= 0) la
            else if (n == 1) xs
            else drop(xs, n-1)
        }
    }

    def dropWhile[A](la: List[A])(f: A => Boolean): List[A] = la match {
        case Cons(x, xs) if f(x) => dropWhile(xs)(f)
        case _ => la
    }
    
    // def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    //     case Nil => a2
    //     case Cons(x, xs) => Cons(x, append(xs, a2))
    // }

    def setHead[A](la: List[A], a: A): List[A] = la match {
        case Nil => Nil
        case Cons(x, xs) => Cons(a, xs)
    }
    
    def init[A](la: List[A]): List[A] = la match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs)) 
    }

    def plusOne(as: List[Int]): List[Int] =
        foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

    def d2s(as: List[Double]): List[String] =
        foldRight(as, Nil: List[String])((a, b) => Cons(a.toString(), b))

    def map[A, B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b) 

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        foldRight(as, Nil: List[B])((a, b) => append(f(a), b))

    def filter2[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(a => if (f(a)) List(a) else Nil)

    def zipWith[A,B,C](as: List[A], bs: List[B])(f:(A, B) => C): List[C] = (as, bs) match {
        case (Cons(a, aa), Cons(b, bb)) => Cons(f(a, b), zipWith(aa, bb)(f))
        case _ => Nil
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

object Main extends App {
    val zw = List.zipWith(List(1, 2, 3), List("a", "b", "c"))((a,b)=>a.toString+b)
    val fm = List.flatMap(List(1,2,3))(a => List(a,a))
    val f2 = List.filter2(List(1,2,3,4,5))(_ % 2 == 0)
    val fi = List.filter(List(1,2,3,4,5))(_ % 2 == 0)
    val mp = List.map(List(1.1,2.2,3.3,4.4,5.5))(_.toString)
    val ds = List.d2s(List(1.1,2.2,3.3,4.4,5.5))
    val po = List.plusOne(List(1,2,3,4,5))
    val re = List.reverse(List(1,2,3,4,5))
    val fl= List.foldLeft(List(1,2,3,4,5), 0)((a, b) => a + b)
    val fr = List.foldRight(List(1,2,3,4,5), 0)((a, b) => a + b)
    val al = List.appendList(List(List(1,2,3,4,5), List(6,7,8,9,10), List(11,12,13,14,15)))
    val ap = List.append(List(1,2,3,4,5), List(6,7,8,9,10))
    val l = List.length(List(1,2,3,4,5))
    println(
        zw
    )
}
