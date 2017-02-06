package fpinscala.solutions

import e3._
import util.e3._
import org.scalatest._

class e3Spec extends FreeSpec with Matchers {

  "list" - {
    def numbers: List[Int] = List(1,2,3,4,5)

    "x" - {
      list.x(numbers) should be (3)
    }

    "tail" - {
      "should return the list without the first element of the list" in {
        list.tail(numbers) should be (List(2, 3, 4, 5))
      }
    }

    "setHead" - {
      "should replace the first element of the list" in {
        list.setHead(7, numbers) should be (List(7, 2, 3, 4, 5))
      }
    }

    "drop" - {
      "should drop the provided n number of elements" in {
        list.drop(numbers, 2) should be (List(3, 4, 5))
        list.drop(numbers, 5) should be (Nil)
        list.drop(numbers, 3) should be (List(4, 5))
      }
    }

    "dropWhile" - {
      "should drop elements while the predicate matches true" in {
        list.dropWhile(numbers, (n: Int) => n < 3) should be (List(3, 4, 5))
        list.dropWhile(numbers, (n: Int) => n > 0) should be (Nil)
        list.dropWhile(numbers, (n: Int) => n < 4) should be (List(4, 5))
      }
    }

    "init" - {
      "should drop the last element in the list" in {
        list.init(numbers) should be (List(1, 2, 3, 4))
        list.init(list.init(numbers)) should be (List(1, 2, 3))
      }
    }

    "foldRight" - {
      "should fold right" in {
        list.foldRight(numbers, 0)(_ - _) should be (3)
      }
    }

    "foldLeft" - {
      "should fold left" in {
        list.foldLeft(numbers, 0)(_ - _) should be (-15)
      }
    }

    "sum" - {
      "should sum all the numbers" in {
        list.sum(numbers) should be (15)
      }
    }

    "product" - {
      "should get the product of all the numbers" in {
        list.product(numbers) should be (120)
      }
    }

    "count" - {
      "should return the number of elements in the list" in {
        list.count(numbers) should be (5)
      }
    }

    "foldLeft" - {
      "should implement sum" in {
        list.sumLeft(numbers) should be (15)
      }

      "should implement product" in {
        list.productLeft(numbers) should be (120)
      }

      "should implement count" in {
        list.countLeft(numbers) should be (5)
      }
    }

    "reverse" - {
      "should reverse the list" in {
        list.reverse(numbers) should be (List(5, 4, 3, 2, 1))
      }
    }

    "append" - {
      "should append a list to another list" in {
        list.append(numbers, List(6, 7, 8)) should be (List(1, 2, 3, 4, 5, 6, 7, 8))
      }
    }

    "flatten" - {
      "should flatten a list of lists" in {
        list.flatten(List(numbers, numbers, List(4, 3, 2, 1))) should be (List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 4, 3, 2, 1))
      }
    }

    "increment" - {
      "should increment the numbers" in {
        list.increment(numbers) should be (List(2, 3, 4, 5, 6))
      }
    }

    "doublesToStrings" - {
      "should convert a list of doubles to a list of strings" in {
        list.doublesToStrings(List(1.3, 3.4)) should be (List("1.3", "3.4"))
      }
    }

    "map" - {
      "should map over items in list in place returning transformed list" in {
        list.map(numbers)(_ * 2) should be (List(2, 4, 6, 8, 10))
      }
    }

    "filter" - {
      "should filter items from a list that do not match predicate" in {
        list.filter(numbers)(_ % 2 == 0) should be (List(2, 4))
      }
    }

    "flatMap" - {
      "should map over items in list, returning new list but merging into flattened list" in {
        list.flatMap(List(1, 2, 3))(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3))
      }
    }

    "combine" - {
      "should combine items from two lists to make a single list" in {
        list.combine(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
        list.combine(List(1, 2, 3), numbers) should be (List(2, 4, 6, 4, 5))
      }
    }

    "zipWith" - {
      "should combine using provided function" in {
        val firstNames = List("Martin", "Paul", "Runar")
        val lastNames = List("Odersky", "Chiusano", "Bjarnason")

        list.zipWith(firstNames, lastNames)((first, last) => s"$first $last") should be (List("Martin Odersky", "Paul Chiusano", "Runar Bjarnason"))
      }
    }

    "startsWith" - {
      "should return true for list [1,2,3,4] starting with [1]" in {
        list.startsWith(List(1,2,3,4), List(1)) should be (true)
      }

      "should return true for list [1,2,3,4] starting with [1,2]" in {
        list.startsWith(List(1,2,3,4), List(1,2)) should be (true)
      }

      "should return true for list [1,2,3,4] starting with [1,2,3]" in {
        list.startsWith(List(1,2,3,4), List(1,2,3)) should be (true)
      }

      "should return true for list [1,2,3,4] starting with [1,2,3,4]" in {
        list.startsWith(List(1,2,3,4), List(1,2,3,4)) should be (true)
      }

      "should return false for list [1,2,3,4] starting with [1,2,3,4,5]" in {
        list.startsWith(List(1,2,3,4), List(1,2,3,4,5)) should be (false)
      }

      "should return false for list [1,2,3,4] starting with [2,3,4]" in {
        list.startsWith(List(1,2,3,4), List(2,3,4)) should be (false)
      }
    }

    "hasSubsequence" - {
      "should return true for list [1, 2, 3, 4] containing subsequence [1, 2]" in {
        list.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be (true)
      }

      "should return true for list [1, 2, 3, 4] containing subsequence [2, 3]" in {
        list.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
      }

      "should return true for list [1, 2, 3, 4] containing subsequence [4]" in {
        list.hasSubsequence(List(1, 2, 3, 4), List(4)) should be (true)
      }

      "should return false for list [1, 2, 3, 4] containing subsequence [3, 1]" in {
        list.hasSubsequence(List(1, 2, 3, 4), List(3, 1)) should be (false)
      }

      "should return false for list [1, 2, 3, 4] containing subsequence [5]" in {
        list.hasSubsequence(List(1, 2, 3, 4), List(5)) should be (false)
      }

      "should return false for list [1, 2, 3, 4] containing subsequence [6, 4]" in {
        list.hasSubsequence(List(1, 2, 3, 4), List(6, 4)) should be (false)
      }
    }
  }

  "tree" - {
    "size" - {
      "should return 1 for single leaf" in {
        tree.size(Leaf(1)) should be (1)
      }

      "should return 3 for branch with two leaves" in {
        tree.size(Branch(Leaf(1), Leaf(2))) should be (3)
      }

      "should return 5 for a branch with one nested branch and one right leaf" in {
        tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be (5)
      }
    }

    "maximum" - {
      "should return the maximum element in tree" in {
        tree.maximum(Leaf(1)) should be (1)
        tree.maximum(Branch(Leaf(1), Leaf(2))) should be (2)
        tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be (3)
        tree.maximum(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3))) should be (7)
      }
    }

    "depth" - {
      "should return the depth of a tree" in {
        tree.depth(Leaf(1)) should be (0)
        tree.depth(Branch(Leaf(1), Leaf(2))) should be (1)
        tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be (2)
        tree.depth(Branch(Branch(Branch(Branch(Leaf(7), Leaf(2)), Leaf(2)), Leaf(2)), Leaf(3))) should be (4)
      }
    }

    "map" - {
      "should apply f to all values in the tree" in {
        tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1) should be (Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
      }
    }

    "fold" - {
      "should fold over all the items in the tree" in {
        tree.fold(Leaf(1))((a: Int) => a)(_ + _) should be (1)
      }
    }
  }
}
