package fpinscala
package solutions
package error_handling

object Exercises {

  /**
   * Exercise 4.2
   *
   * Implement the variance function in terms of flatMap. If the mean
   * of a sequence is m, the variance is the mean of
   * math.pow(x - m, 2) for each element x in the sequence.
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => {
      mean(xs.map(x => math.pow(x - m, 2)))
    })
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
