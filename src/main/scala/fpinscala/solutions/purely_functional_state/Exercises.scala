package fpinscala
package solutions
package purely_functional_state

object Exercises {

  /**
   * Exercise 6.1
   *
   * Write a function that uses RNG.nextInt to generate a random integer between
   * 0 and Int.MaxValue (inclusive). Make sure to handle the corner case when
   * nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt
    val shift = if (n < 0) 1 else 0
    (Math.abs(n) + shift, next)
  }

  /**
   * Exercise 6.2
   *
   * Write a function to generate a Double between 0 and 1, not including 1.
   */
   def double(rng: RNG): (Double, RNG) = {
     val (n, next) = nonNegativeInt(rng)
     (n / (Int.MaxValue.toDouble + 1), next)
   }

  /**
   * Exercise 6.3
   *
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
   * and a (Double, Double, Double) 3-tuple.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, next1) = rng.nextInt
    val (dub, next2) = double(next1)
    ((int, dub), next2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (dub, next1) = double(rng)
    val (int, next2) = next1.nextInt
    ((dub, int), next2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dub1, next1) = double(rng)
    val (dub2, next2) = double(next1)
    val (dub3, next3) = double(next2)
    ((dub1, dub2, dub3), next3)
  }

  /**
   * Exercise 6.4
   *
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    List.fill(count)(())
      .foldRight((List(): List[Int], rng))((_, r) => {
        val (xs, rg) = r
        val (int, next) = nonNegativeInt(rg)
        (xs ++ List(int), next)
      });
}

