package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A,B](t: Tree[A])(f: A => B, g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f,g), fold(r)(f,g))
  }

  def size[A](t: Tree[A]): Int = fold[A, Int](t)((_) => 1, _ + _)

  def maximum(t: Tree[Int]): Int = fold[Int, Int](t)((n) => n, _ max _)

  def depth[A](t: Tree[A]): Int = fold[A, Int](t)((_) => 1, (l,r) => (l max r) + 1)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)((v) => Leaf(f(v)), Branch(_,_))
}
