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
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 3.1
  val x_rer = 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, tail) => tail
    case _ => l
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case _ => l
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(_, tail) if n > 0 => drop(tail, n - 1)
      case _ => l
    }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
      case _ => l
    }

  // Exercise 3.7
  // 应该不行，断言的前提是&&、||，除非实现一个针对乘法的实现

  // Exercise 3.8
  // Cons(1,Cons(2,Cons(3,Cons(4,Nil))))
  // 想法？不就是折叠的本意么？

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, r) => r + 1)


  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    case _ => z
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((r, _) => r + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldRight(l, List(): List[A])((c, r) => Cons(c, r))

  // Exercise 3.13
  // 虽然二刷是自己实现的，但还是觉得太牛逼了
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  foldRight(l, (b: B) => b)((c: A, r) => (g: B) => r(f(g, c)))(z)

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((r, c: A) => (g: B) => r(f(c, g)))(z)

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)((c, r) => Cons(c, r))

  // Exercise 3.15
  def appendALL[A](as: List[List[A]]): List[A] = as match {
    case Nil => Nil
    case Cons(h, Nil) => h
    case Cons(h, Cons(h2, tail)) => appendALL(Cons(append2(h, h2), tail))
  }

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] = l match {
    case Cons(head, tail) => Cons(head + 1, addOne(tail))
    case _ => Nil
  }

  // Exercise 3.17
  def double2Str(l: List[Double]): List[String] = l match {
    case Cons(head, tail) => Cons(head.toString, double2Str(tail))
    case _ => Nil
  }

  // Exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
    case _ => Nil
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(head, tail) if f(head) => Cons(head, filter(tail)(f))
    case Cons(_, tail) => filter(tail)(f)
    case _ => Nil
  }

  def removeOdd(l: List[Int]): List[Int] =
    filter(l)(_ % 2 == 0)

  // Exercise 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldLeft2(l, List(): List[B])((r, c) => append(r, f(c)))

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3.22
  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] =
    l1 match {
      case Cons(head1, tail1) => l2 match {
        case Cons(head2, tail2) => Cons(head1 + head2, zipAdd(tail1, tail2))
        case _ => Nil
      }
      case _ => Nil
    }

  // Exercise 3.23
  // 原来可以(l1, l2)这样去match
  def zipWith[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
    l1 match {
      case Cons(head1, tail1) => l2 match {
        case Cons(head2, tail2) => Cons((head1, head2), zipWith(tail1, tail2))
        case _ => Nil
      }
      case _ => Nil
    }

  // Exercise 3.24
  // 作者的答案更好，主要len是O(n)的复杂度，比较蛋疼
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case list@Cons(_, tail) =>
        val zip = zipWith(sub, list)
        (map(zip)(_._1) == sub && foldLeft(zip, true)((r, c) => r && c._1 == c._2)) ||
          hasSubSequence(tail, sub)
      case _ => false
    }
}
