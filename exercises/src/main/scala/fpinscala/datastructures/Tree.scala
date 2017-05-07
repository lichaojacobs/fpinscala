package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

  // Exercise 3.26
  def maxinum(t: Tree[Int]): Int =
    t match {
      case Leaf(value) => value
      case Branch(left, right) => Math.max(size(left), size(right))
    }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => Math.max(depth(left), depth(right))
    }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(combo: (B, B) => B): B =
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) => combo(fold(left)(f)(combo), fold(right)(f)(combo))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)
  def maxinum2(t: Tree[Int]): Int =
    fold(t)(v => v)(Math.max)
  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(Math.max)
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

}