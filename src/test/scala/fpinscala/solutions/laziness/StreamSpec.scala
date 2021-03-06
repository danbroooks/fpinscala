package fpinscala
package solutions
package laziness

import org.scalatest._

class StreamSpec extends FreeSpec with Matchers {
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

    "works with infinite streams" in {
      Stream.constant(1).take(3).toList should be (List(1, 1, 1))
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

    "works with infinite streams" in {
      Stream.constant(1).takeWhile(_ == 1).take(3).toList should be (List(1, 1, 1))
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

    "works with infinite streams" in {
      Stream.constant(1).map(_ + 2).take(3).toList should be (List(3, 3, 3))
    }
  }

  "filter" - {
    "should filter out elements in the stream" in {
      Stream(1, 2, 3, 4).filter(_ < 3).toList should be (List(1, 2))
    }

    "should do nothing when given an empty stream" in {
      (Stream(): Stream[Int]).filter(_ < 3).toList should be (List())
    }

    "works with infinite streams" in {
      Stream.fibs.filter(_ < 200).take(10).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
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

  "ones" - {
    Stream.ones.take(2).toList should be (List(1, 1))
  }

  "constant" - {
    "creates an infinite stream with a constant value" in {
      Stream.constant(2).take(5).toList should be (List(2, 2, 2, 2, 2))
    }
  }

  "from" - {
    "creates an infinite stream of Ints incrementing by 1" in {
      Stream.from(10).take(3).toList should be (List(10, 11, 12))
    }
  }

  "fibs" - {
    "creates an infinite stream of fibonacci numbers" in {
      Stream.fibs.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
    }
  }

  "unfold" - {
    "should take an initial state and a function for producing the next state and the next value and build a stream from it" in {
      Stream.unfold(64)(n => Some((n, n / 2))).take(7).toList should be (List(64, 32, 16, 8, 4, 2, 1))
    }
  }

  "zipWith" - {
    "should combine using provided function" in {
      val firstNames = Stream("Martin", "Paul", "Runar")
      val lastNames = Stream("Odersky", "Chiusano", "Bjarnason")

      firstNames.zipWith(lastNames)((first, last) => s"$first $last").toList should be (List("Martin Odersky", "Paul Chiusano", "Runar Bjarnason"))
    }
  }

  "zipAll" - {
    "should zip until contents of both stream are exausted" in {
      val words = Stream("One", "Two", "Three")
      val numbers = Stream(1, 2)

      words.zipAll(numbers).toList should be (List(
        (Some("One"), Some(1)),
        (Some("Two"), Some(2)),
        (Some("Three"), None)
      ))
    }
  }

  "startsWith" - {
    "should return true for stream [1,2,3,4] starting with [1]" in {
      Stream(1,2,3,4).startsWith(Stream(1)) should be (true)
    }

    "should return true for stream [1,2,3,4] starting with [1,2]" in {
      Stream(1,2,3,4).startsWith(Stream(1,2)) should be (true)
    }

    "should return true for stream [1,2,3,4] starting with [1,2,3]" in {
      Stream(1,2,3,4).startsWith(Stream(1,2,3)) should be (true)
    }

    "should return true for stream [1,2,3,4] starting with [1,2,3,4]" in {
      Stream(1,2,3,4).startsWith(Stream(1,2,3,4)) should be (true)
    }

    "should return false for stream [1,2,3,4] starting with [1,2,3,4,5]" in {
      Stream(1,2,3,4).startsWith(Stream(1,2,3,4,5)) should be (false)
    }

    "should return false for stream [1,2,3,4] starting with [2,3,4]" in {
      Stream(1,2,3,4).startsWith(Stream(2,3,4)) should be (false)
    }
  }

  "tails" - {
    "returns a stream of suffixes for stream [], starting with the original stream" in {
      Stream().tails.toList.map(_.toList) should be (List(List()))
    }

    "returns a stream of suffixes for stream [1], starting with the original stream" in {
      Stream(1).tails.toList.map(_.toList) should be (List(
        List(1),
        List()
      ))
    }

    "returns a stream of suffixes for stream [1,2], starting with the original stream" in {
      Stream(1,2).tails.toList.map(_.toList) should be (List(
        List(1,2),
        List(2),
        List()
      ))
    }

    "returns a stream of suffixes for stream [1,2,3], starting with the original stream" in {
      Stream(1,2,3).tails.toList.map(_.toList) should be (List(
        List(1,2,3),
        List(2,3),
        List(3),
        List()
      ))
    }
  }

  "scanRight" - {
    "returns a stream of intermediate results" in {
      Stream(1, 2, 3).scanRight(0)(_ + _).toList should be (List(6, 5, 3, 0))
    }
  }
}
