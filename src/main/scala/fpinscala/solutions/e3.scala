package fpinscala.solutions

import annotation.tailrec
import util.e3._

object e3 {

  import List._

  def x(ls: List[Int]): Int = ls match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => sys.error("Cannot apply to empty list")
    case Cons(_, tl) => tl
  }

  def setHead[A](hd: A, ls: List[A]): List[A] = ls match {
    case Nil => sys.error("setCannot apply to empty list")
    case Cons(_, tl) => Cons(hd, tl)
  }

  def drop[A](ls: List[A], n: Int): List[A] =
    if (n <= 0) ls
    else ls match {
      case Nil => Nil
      case Cons(_, tl) => drop(tl, n - 1)
    }

  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Nil => Nil
    case Cons(hd, tl) => if (f(hd)) dropWhile(tl, f) else ls
  }

  def init[A](ls: List[A]): List[A] = ls match {
    case Nil => sys.error("Cannot apply to empty list")
    case Cons(_, Nil) => Nil
    case Cons(hd, tl) => Cons(hd, init(tl))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  @tailrec def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product(ns: List[Int]): Double =
    foldRight(ns, 1.0)(_ * _)

  def count[A](as: List[A]): Int =
    foldRight(as, 0)((_, n) => n + 1)

  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Int]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def countLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)((n, _) => n + 1)

  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, Nil: List[A])((xs, x) => Cons(x, xs))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def flatten[A](lists: List[List[A]]): List[A] =
    foldRight(lists, List[A]())(append)

  def increment(numbers: List[Int]): List[Int] =
    foldRight(numbers, List[Int]())((x, xs) => Cons(x + 1, xs))

  def doublesToStrings(dubs: List[Double]): List[String] =
    foldRight(dubs, List[String]())((x, xs) => Cons(x.toString, xs))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((x, xs) => Cons(f(x), xs))
}
