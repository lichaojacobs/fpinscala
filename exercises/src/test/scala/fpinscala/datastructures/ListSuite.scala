package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * bubei
  * 17/5/6.
  */
@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {

  import List._

  test("match ret") {
    assert(x == x_rer)
  }

  test("reverse list") {
    val l = List(1, 2, 3, 4)
    assert(reverse(reverse(l)) == l)
  }

  test("foldLeft2") {
    val l = List("a", "b", "c")
    assert(foldLeft2(l, "")(_ + _) == "abc")
  }

  test("append2") {
    assert(append2(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
  }

  test("appendALL") {
    assert(appendALL(List(List(1), List(2), List(3))) == List(1, 2, 3))
  }

  test("removeOdd") {
    assert(removeOdd(List(1, 2, 3, 4, 5, 6)) == List(2, 4, 6))
  }

  test("flatMap") {
    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  test("filter2") {
    assert(filter2(List(1, 2, 3))(_ == 2) == List(2))
  }

  test("zipAdd") {
    assert(zipAdd(List(1, 2, 3, 4), List(3, 2, 1)) == List(4, 4, 4))
  }

  test("zipWith") {
    assert(zipWith(List(1), List("a")) == List((1, "a")))
  }

  test("hasSubSequence ==") {
    assert(hasSubSequence(List(1, 2, 3, 4), List(2, 3)))
  }

  test("hasSubSequence !=") {
    assert(!hasSubSequence(List(1, 2, 3, 4), List(3, 2)))
  }

}
