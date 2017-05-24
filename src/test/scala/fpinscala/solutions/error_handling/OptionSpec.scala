package fpinscala
package solutions
package error_handling

import Option._
import org.scalatest._

class OptionSpec extends FreeSpec with Matchers {

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

  "map2" - {
    val sum = (a: Int, b: Int) => a + b

    "should map to none if either argument is None" in {
      map2(Some(5), None)(sum) should be (None)
      map2(None, Some(3))(sum) should be (None)
    }

    "should map the values to single option if both are present" in {
      map2(Some(5), Some(5))(sum) should be (Some(10))
    }
  }

  "traverse" - {
    "should return None if some values fail to be parsed" in {
      traverse(List("22", "hello", "world", "49"))(str => Option.Try(str.toInt)) should be (None)
    }

    "should return a Some of the list of successful values if all values succeeded" in {
      traverse(List("22", "49"))(str => Option.Try(str.toInt)) should be (Some(List(22, 49)))
    }
  }

  "sequence" - {
    "returns list of numbers wrapped in Some, when no None is present" in {
      sequence(List(Some(1), Some(3), Some(5))) should be (Some(List(1, 3, 5)))
    }

    "returns None if single None is present in list" in {
      sequence(List(Some(1), Some(3), None, Some(5))) should be (None)
    }
  }
}
