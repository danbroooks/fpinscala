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

    "headOption" - {
      "returns head in a Some when head exists" in {
        Stream(1, 2, 3, 4).headOption should be (Some(1))
        Stream(1).headOption should be (Some(1))
      }

      "returns None when stream is empty" in {
        Stream().headOption should be (None)
      }
    }

    "map" - {
      "should map over all values in the stream" in {
        Stream(1, 2, 3, 4).map(_ + 1).toList should be (List(2, 3, 4, 5))
      }

      "should do nothing to an empty stream" in {
        (Stream(): Stream[Int]).map(_ + 1).toList should be (List())
      }
    }

    "filter" - {
      "should filter out elements in the stream" in {
        Stream(1, 2, 3, 4).filter(_ < 3).toList should be (List(1, 2))
      }

      "should do nothing when given an empty stream" in {
        (Stream(): Stream[Int]).filter(_ < 3).toList should be (List())
      }
    }

    "append" - {
      "should append another stream to the stream" in {
        Stream(1, 2, 3).append(Stream(4)).toList should be (List(1, 2, 3, 4))
      }

      "should do nothing with empty streams" in {
        (Stream(): Stream[Int]).append(Stream(): Stream[Int]).toList should be (List())
      }
    }

    "flatMap" - {
      "should flat map" in {
        Stream(1, 2, 3, 4).flatMap(n =>
          if (n > 2) Stream(n, n * 2) else Stream()
        ).toList should be (List(3, 6, 4, 8))
      }
    }
  }
}
