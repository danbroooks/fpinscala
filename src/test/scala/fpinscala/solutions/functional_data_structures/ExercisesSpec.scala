package fpinscala
package solutions
package functional_data_structures

import Exercises._
import org.scalatest._

class ExercisesSpec extends FreeSpec with Matchers {

  "x" - {
    x should be (3)
  }

  "ListExercises" - {
    import ListExercises._

    def numbers: List[Int] = List(1,2,3,4,5)

    "tail" - {
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

    "length" - {
      "should return the number of elements in the list" in {
        ListExercises.length(numbers) should be (5)
      }
    }

    "sumLeft" - {
      "should implement sum in terms of foldLeft" in {
        sumLeft(numbers) should be (15)
      }
    }

    "productLeft" - {
      "should implement product in terms of foldLeft" in {
        productLeft(numbers) should be (120)
      }
    }

    "lengthLeft" - {
      "should implement count in terms of foldLeft" in {
        lengthLeft(numbers) should be (5)
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

    "hasSubsequence" - {
      "should return true for list [1, 2, 3, 4] containing subsequence [1, 2]" in {
        hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be (true)
      }

      "should return true for list [1, 2, 3, 4] containing subsequence [2, 3]" in {
        hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
      }

      "should return true for list [1, 2, 3, 4] containing subsequence [4]" in {
        hasSubsequence(List(1, 2, 3, 4), List(4)) should be (true)
      }

      "should return false for list [1, 2, 3, 4] containing subsequence [3, 1]" in {
        hasSubsequence(List(1, 2, 3, 4), List(3, 1)) should be (false)
      }

      "should return false for list [1, 2, 3, 4] containing subsequence [5]" in {
        hasSubsequence(List(1, 2, 3, 4), List(5)) should be (false)
      }

      "should return false for list [1, 2, 3, 4] containing subsequence [6, 4]" in {
        hasSubsequence(List(1, 2, 3, 4), List(6, 4)) should be (false)
      }
    }
  }

  "TreeExercises" - {
    import TreeExercises._

    "size" - {
      "should return 1 for single leaf" in {
        TreeExercises.size(Leaf(1)) should be (1)
      }

      "should return 3 for branch with two leaves" in {
        TreeExercises.size(Branch(Leaf(1), Leaf(2))) should be (3)
      }

      "should return 5 for a branch with one nested branch and one right leaf" in {
        TreeExercises.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be (5)
      }
    }

    "maximum" - {
      "should return the maximum element in tree" in {
        maximum(Leaf(1)) should be (1)
        maximum(Branch(Leaf(1), Leaf(2))) should be (2)
        maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be (3)
        maximum(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3))) should be (7)
      }
    }

    "depth" - {
      "should return the depth of a tree" in {
        depth(Leaf(1)) should be (0)
        depth(Branch(Leaf(1), Leaf(2))) should be (1)
        depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be (2)
        depth(Branch(Branch(Branch(Branch(Leaf(7), Leaf(2)), Leaf(2)), Leaf(2)), Leaf(3))) should be (4)
      }
    }

    "map" - {
      "should apply f to all values in the tree" in {
        map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1) should be (Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
      }
    }

    "fold" - {
      "should fold over all the items in the tree" in {
        fold(Leaf(1))((a: Int) => a)(_ + _) should be (1)
      }
    }
  }
}
