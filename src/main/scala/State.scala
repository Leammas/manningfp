object State extends App {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      n -> nextRNG
    }
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = (RNG) => (A, RNG)

  def ints(c: Int): Rand[List[Int]] = {
    sequence(List.fill(c)(int))
  }

  val int: Rand[Int] = _.nextInt

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, s2) = s(rng)
      (f(a), s2)
    }
  }

  val nnEven: Rand[Int] =
    map(int)(i => i - i % 2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r2) = ra(rng)
      val (b, r3) = rb(r2)
      f(a, b) -> r3
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)(_ -> _)

  val double: Rand[Double] = {
    map(int)(_.toDouble / Int.MaxValue)
  }

  def pure[A](a: A): Rand[A] = rng => a -> rng

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def loop(z: List[Rand[A]])(a: Rand[List[A]]): Rand[List[A]] = {
      z match {
        case Nil => a
        case x :: Nil =>
          map2(x, a)(_ :: _)
        case x1 :: x2 :: xs =>
          val l = map2(x1, x2)(List(_, _))
          val newl = map2(l, a)(_ ++ _)
          loop(xs)(newl)
      }
    }

    loop(fs)(pure(List.empty[A]))
  }

  val rid = both(int, double)

  val a = SimpleRNG(1L)

  println(ints(5)(a))


}
