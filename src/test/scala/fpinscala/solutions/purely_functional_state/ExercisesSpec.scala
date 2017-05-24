package fpinscala
package solutions
package purely_functional_state

import Exercises._
import org.scalatest._

class ExercisesSpec extends FreeSpec with Matchers {
  "nonNegativeInt" - {
    "should return the value of nextInt if the random number is positive" in {
      val generator = SimpleRNG(90)
      nonNegativeInt(generator) should be(generator.nextInt)
    }

    "should keep generating random numbers until result is positive" in {
      val generator = SimpleRNG(-90)
      nonNegativeInt(generator) should be(34627403,SimpleRNG(279205635358137L))
    }
  }

  "double" - {
    "generates a double between 0 and 1, not including 1" in {
      val generator = SimpleRNG(90)
      double(generator) should be(0.016124640125781298,SimpleRNG(2269341352541L))
    }
  }
}
