package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = ???

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = ???

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def reverseMap[A,B](l: List[A])(f: A => B): List[B] = foldLeft[A, List[B]](l, Nil)((xs, x) => Cons(f(x), xs))

  def reverse[A](l: List[A]) = reverseMap(l)((x) => x)

  def map[A,B](l: List[A])(f: A => B): List[B] = reverse(reverseMap(l)(f))

  def addOne(l: List[Int]) = map(l)(_ + 1)

  def allStrings(l: List[Double]) = map(l)(_.toString)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) => filter(xs)(f)
    }

  def flatMap[A,B](as: List[A])(f: A=>List[B]): List[B] = reverse(foldLeft[List[B], List[B]](reverseMap(as)(f), Nil)(append(_,_)))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a) => if (f(a)) List(a) else Nil)

  def zip[A,B](as: List[A], bs: List[B]): List[(A,B)] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a,as), Cons(b,bs)) => Cons((a,b), zip(as, bs))
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = map(zip(as, bs))({case (a,b) => f(a,b)})

  def addLists(l: List[Int], r: List[Int]) = zipWith(l, r)(_ + _)

  @annotation.tailrec
  def isPrefix[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(m, ms), Cons(n,ns)) if m != n => false
    case (Cons(m, ms), Cons(n,ns)) => isPrefix(ms, ns)
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, _) => false
    case _ if isPrefix(sup, sub) => true
    case (Cons(_, ms), _) => hasSubsequence(ms, sub)
  }
}
