import FSM._
import State._

import scala.annotation.tailrec
import scala.collection.immutable.::

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap(x => unit(f(x)))
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State {
    s => {
      val (a, s2) = this.run(s)
      g(a).run(s2)
    }
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      a <- this
      b <- sb
      s <- unit(f(a, b))
    } yield s
  }

  def both[B](sb: State[S, B]): State[S, (A, B)] = map2(sb)(_ -> _)
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => a -> s)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def loop(z: List[State[S, A]])(a: State[S, List[A]]): State[S, List[A]] = {
      z match {
        case Nil => a
        case x :: Nil =>
          x.map2(a)(_ :: _)
        case x1 :: x2 :: xs =>
          val l = x1.map2(x2)(List(_, _))
          val newl = l.map2(a)(_ ++ _)
          loop(xs)(newl)
      }
    }

    loop(fs)(unit(List.empty[A]))
  }
}

object FSM {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def next(input: Input): Machine = {
      input match {
        case Coin =>
          if (locked) {
            copy(locked = false, coins = coins + 1)
          } else {
            copy(coins = coins + 1)
          }
        case Turn =>
          if (!locked) {
            copy(locked = true, candies = candies - 1)
          } else {
            this
          }
      }
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val folded = (m: Machine) => {
      inputs.foldLeft(m)((z, a) => z.next(a))
    }
    State(folded.andThen(newm => (newm.coins, newm.candies) -> newm))
  }


}

object StateEx extends App {

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

  type Rand[A] = State[RNG, A]

  def ints(c: Int): Rand[List[Int]] = {
    sequence(List.fill(c)(int))
  }

  val int: Rand[Int] = State(_.nextInt)

  val nnEven: Rand[Int] =
    int.map(i => i - i % 2)

  val double: Rand[Double] = {
    int.map(_.toDouble / Int.MaxValue)
  }

  assert({
    val a = SimpleRNG(1L)
    ints(5).run(a) == (List(384748, -1151252339, -549383847, 1612966641, -883454042),SimpleRNG(223576932655868L))
  }, "RNG is not reproducable")

  assert({
    val m = Machine(true, 5, 10)
    val cmds: List[Input] = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val ((coins, candies), m2) = simulateMachine(cmds).run(m)
    coins == 14 && candies == 1
  }, "FSM failed!")

}
