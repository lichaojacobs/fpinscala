package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

// Exercise 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a: A) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a: A) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a: A) => f(a)
    case None => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case s: Some[A] => s
  }

  //尴尬...s就是this啊
  def filter(f: A => Boolean): Option[A] = this match {
    case s@Some(a: A) if f(a) => s
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => Math.pow(x - m, 2))))

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(v_a), Some(v_b)) => Some(f(v_a, v_b))
      case _ => None
    }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Some(v) :: tail => sequence(tail) map (v :: _)
    case Nil => Some(Nil)
  }

  // Exercise 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case h :: tail =>
      for {a <- f(h)
           l <- traverse(tail)(f)}
        yield a :: l
    case _ => None
  }
}