package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1
  def toList(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList()
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  // Exercise 5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Empty => true
    case _ => false
  }

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((c, r) => if (p(c)) cons(c, r) else empty)

  // Exercise 5.6
  def headOption: Option[A] =
    this.foldRight(None: Option[A])((c, _) => Some(c))

  // Exercise 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  def map[B](f: A => B): Stream[B] =
    this.foldRight(Empty: Stream[B])((c, r) => cons(f(c), r))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((c, r) => if (f(c)) cons(c, r) else r)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    this.foldRight(s)((c, r) => cons(c, r))

  def flatmap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Empty: Stream[B])((c, r) => f(c).append(r))

  // Exercise 5.13
  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i - 1)))
      case _ => None
    }


  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, other) {
      case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
      case _ => None
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, other) {
      case (Cons(h, t), Cons(h2, t2)) => Some(((Some(h()), Some(h2())), (t(), t2())))
      case (Cons(h, t), _) => Some(((Some(h()), None: Option[B]), (t(), empty)))
      case (_, Cons(h2, t2)) => Some(((None: Option[A], Some(h2())), (empty, t2())))
      case _ => None
    }

  // Exercise 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).foldRight(true) {
      case (_, b) if !b => false
      case ((None, Some(_)), _) => false
      case ((_, None), _) => true
      case ((Some(v1), Some(v2)), _) => v1 == v2
    }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s@Cons(_, t) => Some((s, t()))
      case Empty => None
    } append Stream(empty)

  def hasSubSequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((c, r) => cons(f(c, r.headOption.get), r))

  def scanRight2[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  def scanRight3[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z)){
      case (a:A, t@Cons(h:B, _)) => cons(f(a, h), t)
    }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8
  def constant(n: Int): Stream[Int] = Stream.cons(n, constant(n))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // Exercise 5.10
  def fibs(): Stream[Int] = {
    def fibs(pre: Int, cur: Int): Stream[Int] = {
      Stream.cons(pre, fibs(cur, pre + cur))
    }

    fibs(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map {
      case (a: A, s: S) => Stream.cons(a, unfold(s)(f))
    }.getOrElse(Empty)
  }

  // Exercise 5.12
  def fibs2(): Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constant2(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s)))

  val ones2: Stream[Int] = unfold(1)(s => Some((s, s)))

}