package fpinscala.solutions

import e5._
import util.e5._
import org.scalatest._

class e5Spec extends FreeSpec with Matchers {
  "stream" - {
    "toList" - {
      "converts stream of numbers to a list" in {
        Stream(1, 2, 3, 4).toList should be (List(1, 2, 3, 4))
      }

      "converts empty stream to empty list" in {
        Stream().toList should be (List())
      }
    }

    "take" - {
      "takes first n elements in stream" in {
        Stream(1, 2, 3, 4).take(2).toList should be (List(1, 2))
      }

      "returns empty stream when stream is empty" in {
        Stream().take(2).toList should be (List())
      }
    }

    "drop" - {
      "takes last n elements in stream" in {
        Stream(1, 2, 3, 4).drop(2).toList should be (List(3, 4))
      }

      "returns empty stream when stream is empty" in {
        Stream().drop(2).toList should be (List())
      }
    }

    "takeWhile" - {
      "takes elements while matching predicate" in {
        Stream(1, 2, 3, 4).takeWhile(_ < 3).toList should be (List(1, 2))
        Stream(1, 4, 1, 2).takeWhile(_ < 3).toList should be (List(1))
      }

      "returns empty steam when stream is empty" in {
        (Stream(): Stream[Int]).takeWhile(_ < 3).toList should be (List())
      }
    }

    "forAll" - {
      "returns true when predicate is true for all values" in {
        Stream(1, 2, 3, 4).forAll(_ < 5) should be (true)
      }

      "returns fals when predicate is not true for all values" in {
        Stream(1, 2, 3, 4).forAll(_ == 1) should be (false)
      }
    }
  }
}
