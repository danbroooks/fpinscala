package fpinscala
package solutions
package error_handling

/**
 * Exercise 4.1
 *
 * Implement map, flatMap, getOrElse, orElse and filter on Option
 */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def orElse[B >: A](b: => Option[B]): Option[B] =
    map(Some(_)) getOrElse b

  def filter(f: A => Boolean): Option[A] =
    flatMap(get => if (f(get)) Some(get) else None)
}

object Option {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  /**
   * Exercise 4.3
   *
   * Write a generic function map2 that combines two Option values
   * using a binary function. If either Option value is None, then the
   * return value is too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for { aa <- a; bb <- b } yield f(aa, bb)

  /**
   * Exercise 4.4
   *
   * Write a function sequence that combines a list of Options into
   * one Option containing a list of all the Some values in the
   * original list. If the original list contains None even once, the
   * result of the function should be None; otherwise the result
   * should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  /**
   * Exercise 4.5
   *
   * Implement traverse, then implement sequence in terms of traverse.
   */
  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List()): Option[List[B]])((hd, tl) => map2(f(hd),(tl))(_ :: _))
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
