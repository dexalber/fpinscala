
package fpinscala.datastructures

import Tree._
import org.junit.runner.RunWith
import org.scalatest.{Failed, FunSuite, Pending}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TreeSuite extends FunSuite {
  override def withFixture(test: NoArgTest) =
    super.withFixture(test) match {
      case Failed(ed: NotImplementedError) ⇒ Pending
      case other ⇒ other
    }

  test("size: size(Tree[Int]()) == 0") {
    assert(size(Tree[Int]()) === 0)
  }

  test("size: size(Tree(1)) == 1") {
    assert(size(Tree(1)) === 1)
  }

  test("size: size(Tree(1..10)) == 10") {
    val range = (1 to 10)
    val tree = Tree(range: _*)
    assert(size(tree) === range.length)
  }

  test("size: size(Tree(1..17)) == 17") {
    val range = (1 to 17)
    val tree = Tree(range: _*)
    assert(size(tree) === range.length)
  }

  test("maximum: Empty ⇒ Empty") {
    assert(maximum(Empty) === Empty)
  }

  test("maximum: Tree(1) → 1") {
    assert(maximum(Tree(1)) === Leaf(1))
  }

  test("maximum: Tree(5,1,19,3) → 19") {
    val vals = Seq(5,1,19,3)
    val tree = Tree(vals:_*)
    assert(maximum(tree) === Leaf(vals.max))
  }

  test("maximum: Tree(5,1,19,3, 0) → 19") {
    val vals = Seq(5,1,19,3, 0)
    val tree = Tree(vals:_*)
    assert(maximum(tree) === Leaf(vals.max))
  }

  test("maximum: Branch(Branch(Leaf(1), Leaf(2)), Empty) → 2") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Empty)
    assert(maximum(tree) === Leaf(2))
  }

  test("depth: Empty → 0") {
    assert(depth(Empty) === 0)
  }

  test("depth: Tree(1..7) → 4") {
    val vals = (1 to 7)
    val tree = Tree(vals:_*)
    assert(depth(tree) === 4)
  }
  test("depth: Branch(Branch(Branch(Branch(Leaf(1), Empty), Empty), Empty), Empty) → 5") {
    val tree = Branch(Empty, Branch(Branch(Branch(Leaf(1), Empty), Empty), Empty))
    assert(depth(tree) === 5)
  }

  test("map: Empty → identity → Empty") {
    assert(map(Empty)(identity) === Empty)
  }

  test("map: Empty → (_ + 1) → Empty") {
    assert(map(Empty)(identity) === Empty)
  }

  test("map: Tree(1..7) → identity → Tree(1..7)") {
    val vals = (1 to 7)
    val tree = Tree(vals:_*)
    assert(map(tree)(identity) === tree)
  }

  test("map: Tree(1..7) → (_ + 1) → Tree(2..8)") {
    val vals = (1 to 7)
    val tree = Tree(vals:_*)
    assert(map(tree)(_ + 1) === Tree((2 to 8):_*))
  }

}

