package fpinscala
package solutions
package getting_started

import annotation.tailrec

object Exercises {

  /**
   * Exercise 2.1
   *
   * Write a recursive function to get the nth Fibonacci number. The first two
   * Fibonacci numbers are 0 and 1. The nth number is always the sum of the
   * previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should
   * use a local tail-recursive function.
   */
  @tailrec def fib(n: Int, prev: Int = 0, acc: Int = 1): Int = n match {
    case 0 => n
    case 1 => acc
    case _ => fib(n - 1, acc, acc + prev)
  }

  /**
   * Exercise 2.2
   *
   * Implement isSorted, which checks whether an Array[A] is sorted according to
   * a given comparison function.
   */
  @tailrec def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else ordered(as.head, as.tail.head) && isSorted(as.tail, ordered)
  }

  /**
   * Exercise 2.3
   *
   * Implement curry, which converts a function f of two arguments into a
   * function of one argument that partially applies f.
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  /**
   * Exercise 2.4
   *
   * Implement uncurry, which reverses the transformation of curry. Note that
   * since => associates to the right, A => (B => C) can be written as
   * A => B => C.
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /**
   * Exercise 2.5
   *
   * Implement the higher-order function that composes two functions.
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
