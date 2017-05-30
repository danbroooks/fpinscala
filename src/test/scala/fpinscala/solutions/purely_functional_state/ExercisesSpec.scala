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

  "intDouble" - {
    "generates an int then a double" in {
      val generator = SimpleRNG(90)
      intDouble(generator) should be((34627401,0.4238283708691597),SimpleRNG(221826436403108L))
    }
  }

  "doubleInt" - {
    "generates a double then an int" in {
      val generator = SimpleRNG(90)
      doubleInt(generator) should be((0.016124640125781298,-910164495),SimpleRNG(221826436403108L))
    }
  }

  "double3" - {
    "generates a double then an int" in {
      val generator = SimpleRNG(90)
      double3(generator) should be((0.016124640125781298,0.4238283708691597,0.4347293274477124),SimpleRNG(220292263149791L))
    }
  }

  "ints" - {
    "generates a double then an int" in {
      val generator = SimpleRNG(90)
      ints(2)(generator) should be(List(34627401,910164496),SimpleRNG(221826436403108L))
    }
  }
}
