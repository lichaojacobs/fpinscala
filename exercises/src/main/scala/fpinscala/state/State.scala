package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val pair = rng.nextInt
    (if (pair._1 < 0) -(pair._1 + 1) else pair._1, pair._2)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val pair = nonNegativeInt(rng)
    (pair._1 / (Int.MaxValue.toDouble + 1), pair._2)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft((Nil: List[Int], rng)) {
      case ((l, r), _) =>
        val tuple = nonNegativeInt(r)
        (tuple._1 :: l, tuple._2)
    }
  }

  // Exercise 6.5
  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r) = ra(rng)
      val (b, r2) = rb(r)
      (f(a, b), r2)
    }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldLeft((Nil: List[A], rng)) {
        case ((l, r), rand) =>
          val (a, r2) = rand(r)
          (a :: l, r2)
      }
    }

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  // Exercise 6.9
  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = ???

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
