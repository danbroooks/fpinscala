package fpinscala
package solutions
package purely_functional_state

object Exercises {

  /**
   * Exercise 6.1
   *
   * Write a function that uses RNG.nextInt to generate a random integer between
   * 0 and Int.maxValue (inclusive). Make sure to handle the corner case when
   * nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt
    val shift = if (n < 0) 1 else 0
    (Math.abs(n) + shift, next)
  }
}

