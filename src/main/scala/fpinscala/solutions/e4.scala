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

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List()): Option[List[B]])((hd, tl) => f(hd).map2(tl)(_ :: _))
}
