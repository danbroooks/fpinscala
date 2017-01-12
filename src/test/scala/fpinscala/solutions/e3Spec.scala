package fpinscala.solutions

import e3._
import util.e3._
import org.scalatest._

class e3Spec extends FreeSpec with Matchers {

  def numbers: List[Int] = List(1,2,3,4,5)

  "x" - {
    x(numbers) should be (3)
  }

  "take" - {
    "should return the list without the first element of the list" in {
      tail(numbers) should be (List(2, 3, 4, 5))
    }
  }

  "setHead" - {
    "should replace the first element of the list" in {
      setHead(7, numbers) should be (List(7, 2, 3, 4, 5))
    }
  }

  "drop" - {
    "should drop the provided n number of elements" in {
      drop(numbers, 2) should be (List(3, 4, 5))
      drop(numbers, 5) should be (Nil)
      drop(numbers, 3) should be (List(4, 5))
    }
  }

  "dropWhile" - {
    "should drop elements while the predicate matches true" in {
      dropWhile(numbers, (n: Int) => n < 3) should be (List(3, 4, 5))
      dropWhile(numbers, (n: Int) => n > 0) should be (Nil)
      dropWhile(numbers, (n: Int) => n < 4) should be (List(4, 5))
    }
  }

  "init" - {
    "should drop the last element in the list" in {
      init(numbers) should be (List(1, 2, 3, 4))
      init(init(numbers)) should be (List(1, 2, 3))
    }
  }

  "foldRight" - {
    "should fold right" in {
      foldRight(numbers, 0)(_ - _) should be (3)
    }
  }

  "foldLeft" - {
    "should fold left" in {
      foldLeft(numbers, 0)(_ - _) should be (-15)
    }
  }

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

  "count" - {
    "should return the number of elements in the list" in {
      count(numbers) should be (5)
    }
  }

  "foldLeft" - {
    "should implement sum" in {
      sumLeft(numbers) should be (15)
    }

    "should implement product" in {
      productLeft(numbers) should be (120)
    }

    "should implement count" in {
      countLeft(numbers) should be (5)
    }
  }

  "reverse" - {
    "should reverse the list" in {
      reverse(numbers) should be (List(5, 4, 3, 2, 1))
    }
  }
}
