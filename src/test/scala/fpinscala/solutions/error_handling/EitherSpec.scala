package fpinscala
package solutions
package error_handling

import Either._
import org.scalatest._

class EitherSpec extends FreeSpec with Matchers {

  "map" - {
    val toInt = (n: String) => n.toInt
    "should map values from one type to another" in {
      Right("7").map(toInt) should be (Right(7))
    }

    "should leave left values as they are" in {
      Left("Something went wrong").map(toInt) should be (Left("Something went wrong"))
    }
  }

  "flatMap" - {
    val inc = (n: Int) => Right(n + 1)

    "should map over value when either is Right" in {
      Right(4).flatMap(inc) should be (Right(5))
    }

    "should perform no-op when either is Left" in {
      Left("Something went wrong").flatMap(inc) should be (Left("Something went wrong"))
    }
  }

  "orElse" - {
    "should return Right when either is Right" in {
      Right(4).orElse(Right(7)) should be (Right(4))
    }

    "should return provided fallback when either is Left" in {
      Left("Something went wrong").orElse(Right(7)) should be (Right(7))
    }
  }

  "map2" - {
    val sum = (a: Int, b: Int) => a + b

    "should map to Left if either argument is Left" in {
      Right(7).map2(Left("err"))(sum) should be (Left("err"))
      Left("err").map2(Right(7))(sum) should be (Left("err"))
    }

    "should map to Right if both eithers are Right" in {
      Right(5).map2(Right(5))(sum) should be (Right(10))
    }
  }

  "sequence" - {
    "returns list of numbers wrapped in Right, when no Left is present" in {
      sequence(List(Right(1), Right(3), Right(5))) should be (Right(List(1, 3, 5)))
    }

    "returns first Left if found in list" in {
      val e = new Exception("error!")
      sequence(List(Right(1), Right(3), Left(e), Right(5))) should be (Left(e))
    }
  }

  "traverse" - {
    "should return None if some values fail to be parsed" in {
      traverse(List("22", "hello", "world", "49"))(str => Either.Try(str.toInt)) match {
        case Left(e) => e should have message ("For input string: \"hello\"")
        case _ => fail
      }
    }

    "should return a Some of the list of successful values if all values succeeded" in {
      traverse(List("22", "49"))(str => Either.Try(str.toInt)) should be (Right(List(22, 49)))
    }
  }
}
