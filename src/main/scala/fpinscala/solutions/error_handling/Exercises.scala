package fpinscala
package solutions
package error_handling

object Exercises {

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => {
      mean(xs.map(x => math.pow(x - m, 2)))
    })
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
