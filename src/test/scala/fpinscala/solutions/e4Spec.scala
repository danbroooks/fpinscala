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

  "mean" - {
    "should calculate the mean of a Seq(3, 3, 3)" in {
      mean(Seq(3, 3, 3)) should be (Some(3.0))
    }

    "should calculate the mean of a Seq(5, 4, 3)" in {
      mean(Seq(5, 4, 3)) should be (Some(4.0))
    }

    "should calculate the mean of a Seq(20, 18, 16, 14)" in {
      mean(Seq(20, 18, 16, 14)) should be (Some(17.0))
    }
  }

  "variance" - {
    "should calculate the variance of a Seq(3, 3, 3)" in {
      variance(Seq(3, 3, 3)) should be (Some(0))
    }

    "should calculate the variance of a Seq(5, 10, 5)" in {
      variance(Seq(5, 10, 5)) should be (Some(5.5555555555555545))
    }

    "should calculate the variance of a Seq(20, 18, 16, 14)" in {
      variance(Seq(20, 18, 16, 14)) should be (Some(5.0))
    }
  }
}
