package fpinscala.solutions

import e2._
import org.scalatest._

class e2Spec extends FlatSpec with Matchers {
  "first fibonacci number" should "be 0" in {
    fib(0) should be (0)
  }

  "second fibonacci number" should "be 1" in {
    fib(1) should be (1)
  }

  "third fibonacci number" should "be 1" in {
    fib(2) should be (1)
  }

  "fourth fibonacci number" should "be 2" in {
    fib(3) should be (2)
  }

  "fifth fibonacci number" should "be 3" in {
    fib(4) should be (3)
  }

  "sixth fibonacci number" should "be 5" in {
    fib(5) should be (5)
  }

  "seventh fibonacci number" should "be 8" in {
    fib(6) should be (8)
  }

  "eighth fibonacci number" should "be 13" in {
    fib(7) should be (13)
  }

  "nineth fibonacci number" should "be 21" in {
    fib(8) should be (21)
  }

  "tenth fibonacci number" should "be 34" in {
    fib(9) should be (34)
  }

  "eleventh fibonacci number" should "be 55" in {
    fib(10) should be (55)
  }

  "twelth fibonacci number" should "be 89" in {
    fib(11) should be (89)
  }
}
