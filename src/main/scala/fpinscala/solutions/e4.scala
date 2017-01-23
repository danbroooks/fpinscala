package fpinscala.solutions

import util.e4._

object e4 {
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => {
      mean(xs.map(x => math.pow(x - m, 2)))
    })
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))
}
