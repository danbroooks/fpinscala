package fpinscala.solutions

import annotation.tailrec

object e2 {

  @tailrec def fib(n: Int, prev: Int = 0, acc: Int = 1): Int = n match {
    case 0 => n
    case 1 => acc
    case _ => fib(n - 1, acc, acc + prev)
  }

  @tailrec def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else ordered(as.head, as.tail.head) && isSorted(as.tail, ordered)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)
}

