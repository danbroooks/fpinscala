package fpinscala
package solutions
package error_handling

import Exercises._
import org.scalatest._

class ExercisesSpec extends FreeSpec with Matchers {

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
