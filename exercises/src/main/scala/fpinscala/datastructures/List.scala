package fpinscala.datastructures

import java.lang.UnsupportedOperationException

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

  def apply[A](as: A*): List[A] = {
    @annotation.tailrec
    def loop(xxs: Seq[A], accum: List[A]): List[A] = 
      if (xxs.isEmpty) accum
      else loop(xxs.tail, Cons(xxs.head, accum))

    if (as.isEmpty) Nil
    else loop(as.reverse, List[A]())
  }

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

  def head[A](l: List[A]) = l match {
    case Cons(h, _) ⇒ h
    case _ ⇒ Nil
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) ⇒ t
    case _ ⇒ Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) ⇒ Cons(h, t)
    case _ ⇒ Nil
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else if (l == Nil) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], pred: A => Boolean): List[A] = l match {
    case Cons(h, t) if pred(h) ⇒ dropWhile(t, pred)
    case _ ⇒ l
  }

  def take[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Cons(x, xs), n) if n > 0 ⇒ Cons(x, take(xs, n - 1))
    case _ ⇒ Nil
  }

  def takeAtLeast[A](l: List[A], n: Int)(implicit accum: List[A] = Nil): List[A] = (l, n) match {
    case (Cons(x, xs), n) if n > 0 ⇒ takeAtLeast(xs, n - 1)(Cons(x, accum))
    case (_, 0) ⇒ reverse(accum)
    case _ ⇒ Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) ⇒ Nil
    case Cons(h, t) ⇒ Cons(h, init(t))
    case _ ⇒ Nil
  }


  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil ⇒ z
    case Cons(h, t) ⇒ foldLeft(t, f(z, h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, List[B]()){(h, accum) ⇒ Cons(f(h), accum) }

  def sum3(l: List[Int]) = foldLeft(l, 0)((accum, h) ⇒ accum + h)
  def product3(l: List[Int]) = foldLeft(l, 1)((accum, h) ⇒ accum * h)

  def length[A](l: List[A]): Int = foldLeft(l, 0){ (accum, _) ⇒ accum + 1}

  def reverse[A](l: List[A]) = foldLeft(l, Nil:List[A]){ (accum, h) ⇒ Cons(h, accum)}

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B =
    foldRight(reverse(l), z){(h, accum) ⇒ f(accum, h)}

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(reverse(l), z){(accum, h) ⇒ f(h, accum)}

  def append2[E1, E2 >: E1](xxs: List[E1], yys: List[E2]): List[E2] =
    foldLeft(reverse(xxs), yys)((accum, h) ⇒ Cons(h, accum))

  def flatten[A](lss: List[List[A]]): List[A] = foldLeft(reverse(lss), List[A]())((a, b) ⇒ append(b, a))

  def incrList(xs: List[Int]): List[Int] = xs match {
    case Cons(h, t) ⇒ Cons(h + 1, incrList(t))
    case _ ⇒ Nil
  }

  def dblList2Str(xs: List[Double]) = map(xs)(_.toString)

  def filter[A](xs: List[A])(pred: A ⇒ Boolean): List[A] =
    foldRight(xs, List[A]())((h, accum) ⇒ if (pred(h)) Cons(h, accum) else accum)

  def flatMap[A,B](as: List[A])(f: A ⇒ List[B]): List[B] =
    flatten(map(as)(f))

  def filter2[A](xs: List[A])(pred: A ⇒ Boolean): List[A] = 
    flatMap(xs)(x ⇒ if (pred(x)) List(x) else Nil)

  def zipWith[A, B, C](xxs: List[A], yys: List[B])(f: (A, B) ⇒ C): List[C] = (xxs, yys) match {
    case (Cons(x, xs), Cons(y, ys)) ⇒ Cons(f(x, y), zipWith(xs, ys)(f))
    case _ ⇒ Nil
  }
    

  def pairwiseAdd(a: List[Int], b: List[Int]) = zipWith(a, b)(_ + _)

  def subseqs[A](seq: List[A], n: Int)(implicit z: List[List[A]] = Nil): List[List[A]] = {
    if (seq == Nil || n == 0)
      return reverse(z)

    val sub = takeAtLeast(seq, n)
    if (sub == Nil) reverse(z)
    else subseqs(tail(seq), n)(Cons(sub, z))
  }
      
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, _) ⇒ false
    case (_, Nil) ⇒ true
    case (xs: Cons[A], ys) ⇒
      Nil != dropWhile(subseqs(sup, length(sub)), ((_: List[A]) != sub))
  }

}

