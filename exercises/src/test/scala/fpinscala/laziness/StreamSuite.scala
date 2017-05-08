package fpinscala.laziness

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * bubei
  * 17/5/7.
  */
@RunWith(classOf[JUnitRunner])
class StreamSuite extends FunSuite {

  test("to List") {
    assert(Stream(1, 2, 3).toList() == List(1, 2, 3))
  }

  test("take") {
    assert(Stream(1, 2, 3).take(2).toList() == List(1, 2))
  }

  test("drop") {
    assert(Stream(1, 2, 3).drop(1).toList() == List(2, 3))
  }

  test("takeWhile") {
    assert(Stream(1, 2, 3, 1).takeWhile(_ < 3).toList() == List(1, 2))
  }

  test("takeWhile2") {
    assert(Stream(1, 2, 3, 1).takeWhile2(_ < 3).toList() == List(1, 2))
  }

  test("headOption") {
    assert(Stream(1, 2, 3).headOption.contains(1))
  }

  test("headOption empty") {
    assert(Empty.headOption.isEmpty)
  }

  test("fibs") {
    assert(Stream.fibs().take(7).toList() == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("fibs2") {
    assert(Stream.fibs2().take(7).toList() == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("take2") {
    assert(Stream(1, 2, 3).take2(2).toList() == List(1, 2))
  }

  test("takeWhile3") {
    assert(Stream(1, 2, 3, 1).takeWhile3(_ < 3).toList() == List(1, 2))
  }

  test("zipAll") {
    assert(Stream(1, 2).zipAll(Stream("a")).toList() == List((Some(1),Some("a")), (Some(2),None)))
  }

  test("startsWith") {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    assert(Stream(1, 2).startsWith(Stream(1, 2)))
    assert(!Stream(1, 2).startsWith(Stream(1, 2, 3)))
    assert(!Stream(2, 3, 4).startsWith(Stream(1, 2, 3)))
    assert(Stream(2, 3, 4).startsWith(Empty))
    assert(Empty.startsWith(Empty))
    assert(!Empty.startsWith(Stream(1)))
  }

  test("tails") {
    assert(Stream(1,2,3).tails.toList().map(_.toList()) ==
      List(List(1,2,3),List(2,3),List(3),List()))
  }

  test("scanRight") {
    assert(Stream(1,2,3).scanRight(0)(_ + _).toList() ==
      List(6, 5, 3, 0))
    assert(Stream(1,2,3).scanRight2(0)(_ + _).toList() ==
      List(6, 5, 3, 0))
    assert(Stream(1,2,3).scanRight3(0)(_ + _).toList() ==
      List(6, 5, 3, 0))
  }

}
