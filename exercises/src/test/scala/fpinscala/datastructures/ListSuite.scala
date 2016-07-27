package fpinscala.datastructures

import List._
import org.junit.runner.RunWith
import org.scalatest.{Failed, FunSuite, Pending}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {
  override def withFixture(test: NoArgTest) =
    super.withFixture(test) match {
      case Failed(ed: NotImplementedError) ⇒ Pending
      case other ⇒ other
    }

  test("sum: sum of empty list should be 0") {
    assert(sum(Nil) === 0)
  }

  test("sum: sum of single-element list should be the elemen") {
    assert(sum(List(5)) === 5)
  }

  test("sum: sum of list should be sum of its element") {
    assert(sum(List(1, 2, 3, 4)) === 10)
  }

  test("sum2: sum2 of empty list should be 0") {
    assert(sum2(Nil) === 0)
  }

  test("sum2: sum2 of single-element list should be the elemen") {
    assert(sum2(List(5)) === 5)
  }

  test("sum2: sum2 of list should be sum2 of its element") {
    assert(sum2(List(1, 2, 3, 4)) === 10)
  }

  test("product: product of empty list should be 1.0") {
    assert(product(Nil) === 1.0)
  }
  test("product: product of single-element list should be the element") {
    assert(product(List(7.0)) === 7.0)
  }
  test("product: product of list should be product of its elements") {
    assert(product(List(1.0, 2.0, 3.0, 4.0)) === 24.0)
  }
  test("product: product of list containing zero should be zero") {
    assert(product(List(1.0, 2.0, 0.0, 4.0)) === 0.0)
  }

  test("product2: product2 of empty list should be 1.0") {
    assert(product2(Nil) === 1.0)
  }
  test("product2: product2 of single-element list should be the element") {
    assert(product2(List(7.0)) === 7.0)
  }
  test("product2: product2 of list should be product2 of its elements") {
    assert(product2(List(1.0, 2.0, 3.0, 4.0)) === 24.0)
  }
  test("product2: product2 of list containing zero should be zero") {
    assert(product2(List(1.0, 2.0, 0.0, 4.0)) === 0.0)
  }

  test("append: append of two empty lists should be empty list") {
    assert(append(Nil, Nil) === Nil)
  }
  test("append: append of empty list to a list should be list") {
    assert(append(Nil, List(3)) === List(3))
  }
  test("append: append of list to empty list should be list") {
    assert(append(List(3), Nil) === List(3))
  }
  test("append: append of list to one-element list should be concatenation of lists") {
    assert(append(List(1, 2), List(3)) === List(1, 2, 3))
  }
  test("append: append of one-element list to list should be concatenation of lists") {
    assert(append(List(1), List(2, 3)) === List(1, 2, 3))
  }
  test("append: append of two lists should be concatenation of lists") {
    assert(append(List(1, 2), List(3, 4)) === List(1, 2, 3, 4))
  }

  test("tail: tail of Nil should be Nil") {
    assert(tail(Nil) === Nil)
  }
  test("tail: tail of single-element list should be Nil") {
    assert(tail(List(3)) === Nil)
  }
  test("tail: tail of list should be rest") {
    assert(tail(List(1, 2, 3)) === List(2, 3))
  }

  test("setHead: setHead of empty list should be empty list") {
    assert(setHead(Nil, 1) === Nil)
  }
  test("setHead: setHead of single-element list should be two-element list") {
    assert(setHead(List(2), 1) === List(1))
  }
  test("setHead: setHead of two-element list should be three-element list") {
    assert(setHead(List(3, 2), 1) === List(1, 2))
  }

  test("drop: drop of zero elements from empty list is empty list") {
    assert(drop(Nil, 0) === Nil)
  }
  test("drop: drop of one element from empty list is empty list") {
    assert(drop(Nil, 1) === Nil)
  }
  test("drop: drop of many elements from empty list is empty list") {
    assert(drop(Nil, 10) === Nil)
  }
  test("drop: drop of zero elements from single-element list is the list") {
    assert(drop(List(3), 0) === List(3))
  }
  test("drop: drop of one element from single-element list is empty list") {
    assert(drop(List(3), 1) === Nil)
  }
  test("drop: drop of many elements from single-element list is empty list") {
    assert(drop(List(3), 10) === Nil)
  }
  test("drop: drop of zero elements from list is list") {
    assert(drop(List(1, 2, 3), 0) === List(1, 2, 3))
  }
  test("drop: drop of one elements from list is list without 1st element") {
    assert(drop(List(1, 2, 3), 1) === List(2, 3))
  }
  test("drop: drop of n elements from list is list without 1st n elements") {
    assert(drop(List(1, 2, 3), 2) === List(3))
  }
  test("drop: drop of too many elements from list is empty list") {
    assert(drop(List(1, 2, 3), 10) === Nil)
  }

  val positive = (x: Int) => x > 0

  test("dropWhile: dropWhile of empty list should be empty list") {
    assert(dropWhile(Nil, positive) === Nil)
  }
  test("dropWhile: dropWhile of list with single valid element should be empty list") {
    assert(dropWhile(List(1), positive) === Nil)
  }
  test("dropWhile: dropWhile of list with only valid elements should be empty list") {
    assert(dropWhile(List(1, 2, 3, 4), positive) === Nil)
  }
  test("dropWhile: dropWhile of list with two leading valid elements should be list without leading elements") {
    assert(dropWhile(List(1, 2, -3, 4), positive) === List(-3, 4))
  }
  test("dropWhile: dropWhile of list with one leading valid element should be list without leading element") {
    assert(dropWhile(List(1, -2, -3, 4), positive) === List(-2, -3, 4))
  }
  test("dropWhile: dropWhile of list with no leading valid elements should be same list") {
    assert(dropWhile(List(-1, -2, -3, 4), positive) === List(-1, -2, -3, 4))
  }
  test("dropWhile: dropWhile of list with no valid elements should be Nil") {
    assert(dropWhile(List(-1, -2, -3, -4), positive) === List(-1, -2, -3, -4))
  }

  test("init: init of empty list should be empty list") {
    assert(init(Nil) === Nil)
  }
  test("init: init of single-element-list should be empty list") {
    assert(init(List(3)) === Nil)
  }
  test("init: init of list should not have last element") {
    assert(init(List(1, 2, 3)) === List(1, 2))
  }

  test("length: length of empty list is zero") {
    assert(length(Nil) === 0)
  }
  test("length: length of single-element list is one") {
    assert(length(List(1)) === 1)
  }
  test("length: length of n-element list is n") {
    assert(length(List(1, 2, 3)) === 3)
  }

  test("foldLeft: should compute the same sum value as foldRight") {
    assert(foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) ===
      foldRight(List(1, 2, 3, 4, 5), 0)(_ + _))
  }

  test("foldLeft: should compute the same product value as foldRight") {
    assert(foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _) ===
      foldRight(List(1, 2, 3, 4, 5), 1)(_ * _))
  }

  test("foldLeft: should compute the same concatenation value as foldRight") {
    assert(foldLeft(List("a", "b", "c"), "")(_ + _) ===
      foldRight(List("a", "b", "c"), "")(_ + _))
  }
}

