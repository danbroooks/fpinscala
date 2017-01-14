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

  "append" - {
    "should append a list to another list" in {
      append(numbers, List(6, 7, 8)) should be (List(1, 2, 3, 4, 5, 6, 7, 8))
    }
  }

  "flatten" - {
    "should flatten a list of lists" in {
      flatten(List(numbers, numbers, List(4, 3, 2, 1))) should be (List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 4, 3, 2, 1))
    }
  }

  "increment" - {
    "should increment the numbers" in {
      increment(numbers) should be (List(2, 3, 4, 5, 6))
    }
  }

  "doublesToStrings" - {
    "should convert a list of doubles to a list of strings" in {
      doublesToStrings(List(1.3, 3.4)) should be (List("1.3", "3.4"))
    }
  }

  "map" - {
    "should map over items in list in place returning transformed list" in {
      map(numbers)(_ * 2) should be (List(2, 4, 6, 8, 10))
    }
  }

  "filter" - {
    "should filter items from a list that do not match predicate" in {
      filter(numbers)(_ % 2 == 0) should be (List(2, 4))
    }
  }

  "flatMap" - {
    "should map over items in list, returning new list but merging into flattened list" in {
      flatMap(List(1, 2, 3))(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3))
    }
  }

  "combine" - {
    "should combine items from two lists to make a single list" in {
      combine(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
      combine(List(1, 2, 3), numbers) should be (List(2, 4, 6, 4, 5))
    }
  }

  "zipWith" - {
    "should combine using provided function" in {
      val firstNames = List("Martin", "Paul", "Runar")
      val lastNames = List("Odersky", "Chiusano", "Bjarnason")

      zipWith(firstNames, lastNames)((first, last) => s"$first $last") should be (List("Martin Odersky", "Paul Chiusano", "Runar Bjarnason"))
    }
  }
}
