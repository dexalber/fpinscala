
package fpinscala.errorhandling

import Option._
import org.junit.runner.RunWith
import org.scalatest.{Failed, FunSuite, Pending}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OptionSuite extends FunSuite {
  override def withFixture(test: NoArgTest) =
    super.withFixture(test) match {
      case Failed(ed: NotImplementedError) ⇒ Pending
      case other ⇒ other
    }

  def pred(a: Boolean) = a
  def fn(a: Boolean): Option[Boolean] = if (a) Some(a) else None
  test("map: identity") {
    assert((Some(true) map identity) === Some(true))
  }
  test("map: identity'") {
    assert((Some(true) map (!identity(_:Boolean))) === Some(false))
  }
  test("map: None") {
    assert((None map identity) === None)
  }

  test("getOrElse") {
    val op = Some(true)
    assert(op getOrElse false)
  }
  test("getOrElse 2") {
    val op = None
    assert(op getOrElse true)
  }

  test("flatMap") {
    val op = Some(true)
    assert((op flatMap fn) === op)
  }

  test("flatMap 2") {
    val op = Some(false)
    assert((op flatMap fn) === None)
  }

  test("flatMap 3") {
    val op = None
    assert((op flatMap fn) === None)
  }

  test("orElse") {
    val op = Some(true)
    assert((op orElse None) === op)
  }

  test("orElse 2") {
    val op = None
    assert((op orElse Some(true)) === Some(true))
  }

  test("filter") {
    val op = Some(true)
    assert((op filter identity) === op)
  }

  test("filter 2") {
    val op = Some(false)
    assert((op filter identity) === None)
  }

  test("filter 3") {
    val op = None
    assert((op filter identity) === None)
  }
}
