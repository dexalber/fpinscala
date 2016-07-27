package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case object Empty extends Tree[Nothing]

object Tree {
  private def fromVector[A](xxs: Vector[A]): Tree[A] = xxs match {
    case Vector() ⇒ Empty
    case x +: Vector() ⇒ Leaf(x)
    case _ ⇒ {
      val m: Int = xxs.length / 2
      Branch(fromVector(xxs take m), fromVector(xxs drop m))
    }

  }
  def apply[A](xxs: A*): Tree[A] = fromVector(xxs.toVector)
  def size[A](t: Tree[A]): Int = t match {
    case l: Leaf[A] ⇒ 1
    case b: Branch[A] ⇒ size(b.left) + size(b.right)
    case Empty ⇒ 0
  }
  def maximum(t: Tree[Int]): Tree[Int] = t match {
    case Branch(l, r) ⇒ {
      (maximum(l), maximum(r)) match {
        case (l, Empty) ⇒ l
        case (Empty, r) ⇒ r
        case (Leaf(l), Leaf(r)) ⇒
          if (l >= r) Leaf(l)
          else Leaf(r)
        case _ ⇒ throw new AssertionError("unreachable")
      }
    }
    case leaf: Leaf[Int] ⇒ leaf
    case Empty ⇒ Empty
  }
  def depth[A](t: Tree[A]): Int = t match {
    case Empty ⇒ 0
    case _: Leaf[A] ⇒ 1
    case Branch(l, r) ⇒ math.max(1 + depth(l), 1 + depth(r))
  }
  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Empty ⇒ Empty
    case Leaf(v) ⇒ Leaf(f(v))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }
  def fold[A, B](t: Tree[A], z: B)(f: (B, A) ⇒ B): B = t match {
    case Empty ⇒ z
    case Leaf(v) ⇒ f(z, v)
    case Branch(l, r) ⇒ fold(l, fold(r, z)(f))(f)
  }

  def size2[A](t: Tree[A]): Int =  fold(t, 0)((acc, x) ⇒ acc + 1)
  def maximum2(t: Tree[Int]): Tree[Int] = fold(t, Tree[Int]()){
    case (Empty, x) ⇒ Leaf(x)
    case (Leaf(a), b) ⇒ Leaf(a max b)
  }
  def depth2[A](t: Tree[A]): Int = ???
  def map2[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = ???
}

