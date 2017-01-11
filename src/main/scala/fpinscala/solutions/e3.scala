package fpinscala.solutions

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
    case Nil => sys.error("tail of empty list")
    case Cons(_, tl) => tl
  }

  def setHead[A](hd: A, ls: List[A]): List[A] = ls match {
    case Nil => sys.error("setHead on empty list")
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
}
