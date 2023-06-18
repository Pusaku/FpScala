sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r) 
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }

    def map[A,B](t: Tree[A])(f:A => B): Tree[B] = t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    } 

    def size2[A](t: Tree[A]): Int = 
        fold(t)(a => 1)((b1, b2) => b1 + b2 + 1)

    def maximun2(t: Tree[Int]): Int =
        fold(t)(a => a)((b1, b2) => b1 max b2)

    def depth2[A](t: Tree[A]): Int =
        fold(t)(a => 1)((b1, b2) => (b1 max b2) + 1)

    def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
        fold(t)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))
}


object Main extends  App {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3))
    val f = (x: Int) => x * 10
    println(Tree.map2(tree)(f))
}