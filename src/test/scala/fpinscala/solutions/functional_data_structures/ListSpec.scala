package fpinscala
package solutions
package functional_data_structures

import org.scalatest._
import List._

class ListSpec extends FreeSpec with Matchers {

  def numbers: List[Int] = List(1,2,3,4,5)

  "sum" - {
    "should sum all the numbers" in {
      sum(numbers) should be (15)
    }
  }

  "product" - {
    "should get the product of all the numbers" in {
      product(numbers) should be (120)
    }
  }

  "startsWith" - {
    "should return true for list [1,2,3,4] starting with [1]" in {
      startsWith(List(1,2,3,4), List(1)) should be (true)
    }

    "should return true for list [1,2,3,4] starting with [1,2]" in {
      startsWith(List(1,2,3,4), List(1,2)) should be (true)
    }

    "should return true for list [1,2,3,4] starting with [1,2,3]" in {
      startsWith(List(1,2,3,4), List(1,2,3)) should be (true)
    }

    "should return true for list [1,2,3,4] starting with [1,2,3,4]" in {
      startsWith(List(1,2,3,4), List(1,2,3,4)) should be (true)
    }

    "should return false for list [1,2,3,4] starting with [1,2,3,4,5]" in {
      startsWith(List(1,2,3,4), List(1,2,3,4,5)) should be (false)
    }

    "should return false for list [1,2,3,4] starting with [2,3,4]" in {
      startsWith(List(1,2,3,4), List(2,3,4)) should be (false)
    }
  }
}
