package fpinscala
package solutions
package purely_functional_state

import RNG.SimpleRNG
import org.scalatest._

class RNGSpec extends FreeSpec with Matchers {
  "map2" - {
    "should map 2 Rand actions and combine them" in {
      val generator = SimpleRNG(90)
      val expected = (34627401 + (-910164495),SimpleRNG(221826436403108L))
      RNG.map2(RNG.int, RNG.int)(_ + _)(generator) should be(expected)
    }
  }

  "sequence" - {
    import RNG.sequence

    "do nothing when given an empty list" in {
      val generator = SimpleRNG(90)
      val expected = (List(), generator)
      sequence(List())(generator) should be (expected)
    }

    "sequence a list of two" in {
      val generator = SimpleRNG(90)
      val expected = (List(34627401, -910164495), SimpleRNG(221826436403108L))
      sequence(List(RNG.int, RNG.int))(generator) should be (expected)
    }

    "sequence a list of three" in {
      val generator = SimpleRNG(90)
      val expected = (List(34627401, -910164495, -933574121), SimpleRNG(220292263149791L))
      sequence(List(RNG.int, RNG.int, RNG.int))(generator) should be (expected)
    }
  }
}
