package fpinscala
package solutions
package purely_functional_state

import RNG.SimpleRNG
import org.scalatest._

class RNGSpec extends FreeSpec with Matchers {
  "map2" - {
    "should map 2 Rand actions and combine them" in {
      val generator = SimpleRNG(90)
      val expected = (34627401 + 910164495,SimpleRNG(221826436403108L))
      RNG.map2(RNG.int, RNG.int)(_ - _)(generator) should be(expected)
    }
  }
}
