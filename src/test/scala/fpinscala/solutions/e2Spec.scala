package fpinscala.solutions

import e2._
import org.scalatest._

class e2Spec extends FreeSpec with Matchers {
  "fib" - {
    "first fibonacci number should be 0" in {
      fib(0) should be (0)
    }

    "second fibonacci number should be 1" in {
      fib(1) should be (1)
    }

    "third fibonacci number should be 1" in {
      fib(2) should be (1)
    }

    "fourth fibonacci number should be 2" in {
      fib(3) should be (2)
    }

    "fifth fibonacci number should be 3" in {
      fib(4) should be (3)
    }

    "sixth fibonacci number should be 5" in {
      fib(5) should be (5)
    }

    "seventh fibonacci number should be 8" in {
      fib(6) should be (8)
    }

    "eighth fibonacci number should be 13" in {
      fib(7) should be (13)
    }

    "nineth fibonacci number should be 21" in {
      fib(8) should be (21)
    }

    "tenth fibonacci number should be 34" in {
      fib(9) should be (34)
    }

    "eleventh fibonacci number should be 55" in {
      fib(10) should be (55)
    }

    "twelth fibonacci number should be 89" in {
      fib(11) should be (89)
    }
  }

  "isSorted" - {
    "checks whether an Array[A] is sorted according to a given compare function" in {
      isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b) should be(true)
      isSorted(Array(3, 2, 1), (a: Int, b: Int) => a < b) should be(false)
      isSorted(Array(6, 5, 7, 4, 1, 0), (a: Int, b: Int) => a < b) should be(false)

      isSorted(Array(("Dan", 27), ("Mike", 30), ("Alan", 32)), (a: (String, Int), b: (String, Int)) => a._2 < b._2) should be(true)

      isSorted(Array("Alan", "Dan", "Mike"), (a: String, b: String) => a < b) should be(true)
    }
  }

  "curry" - {
    val uncurried = (num: Int, bool: Boolean) => s"number is $num and boolean is $bool"
    curry(uncurried)(18)(true) should be("number is 18 and boolean is true")
  }

  "uncurry" - {
    val curried = (num: Int) => (bool: Boolean) => s"number is $num and boolean is $bool"
    uncurry(curried)(18, true) should be("number is 18 and boolean is true")
  }

  "compose" - {
    val first = (name: String) => s"Hello, $name"
    val second = (str: String) => s"$str! Welcome."
    compose(first, second)("Dan") should be("Hello, Dan! Welcome.")
  }
}
