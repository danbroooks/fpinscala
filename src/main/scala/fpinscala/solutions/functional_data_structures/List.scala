package fpinscala
package solutions
package functional_data_structures

import annotation.tailrec
import Exercises.ListExercises._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product(ns: List[Int]): Double =
    foldRight(ns, 1.0)(_ * _)

  @tailrec def startsWith[A](list: List[A], check: List[A]): Boolean = (list, check) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) => (x == y) && startsWith(xs, ys)
    case _ => false
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
