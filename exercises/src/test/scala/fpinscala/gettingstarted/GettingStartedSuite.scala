package fpinscala.gettingstarted

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GettingStartedSuite extends FunSuite {

  import MyModule._
  import PolymorphicFunctions._

  test("5! is 120") {
    assert(factorial(5) == 120)
  }

  test("The 7th item in the fibonacci sequence is 13") {
    assert(fib(7) == 13)
  }

  test("[0, 1, 2, 3, 4] is sorted in ascending order") {
    val test = (0 to 4).toArray
    assert(isSorted(test, (x: Int, y: Int) => x <= y))
  }

  test("[0, 1, 2, 3, 4] is sorted in descending order") {
    val test = (4 to 0).toArray
    assert(isSorted(test, (x: Int, y: Int) => x >= y))
  }

  test("[1, 0, 2, 3, 4] is not sorted") {
    val shuffled = Array(1, 0, 2, 3, 4)
    assert(!isSorted(shuffled, (x: Int, y: Int) => x <= y))
  }

  test("Function composition is the reverse of andThen") {
    val f = (x: Double) => math.Pi / 2 - x
    val cos = f andThen math.sin
    assert(cos(9) == compose(math.sin, f)(9))
  }

}