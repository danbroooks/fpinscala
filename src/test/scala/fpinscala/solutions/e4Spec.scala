package fpinscala.solutions

import e4._
import util.e4._
import org.scalatest._

class e4Spec extends FreeSpec with Matchers {
  "option" - {
    "map" - {
      "should map over value encapsulated by some" in {
        Some(4).map((n: Int) => n + 1) should be (Some(5))
      }

      "should perform no-op when option is none" in {
        None.map((n: Int) => n + 1) should be (None)
      }
    }

    "flatMap" - {
      "should map over value encapsulated by some" in {
        Some(4).flatMap((n: Int) => Some(n + 1)) should be (Some(5))
      }

      "should perform no-op when option is none" in {
        None.flatMap((n: Int) => Some(n + 1)) should be (None)
      }
    }

    "getOrElse" - {
      "when value is a some, should return that value" in {
        Some(4).getOrElse(9) should be (4)
      }

      "when value is a none, should return fallback value" in {
        None.getOrElse(9) should be (9)
      }
    }

    "orElse" - {
      "when value is a some, should return that some" in {
        Some(4).orElse(Some(9)) should be (Some(4))
      }

      "when value is a none, should return alternative option" in {
        None.orElse(Some(9)) should be (Some(9))
      }
    }

    "filter" - {
      "should filter the values inside the Option" in {
        Some(4).filter(_ > 3) should be (Some(4))
        Some(4).filter(_ < 3) should be (None)
      }
    }
  }
}
