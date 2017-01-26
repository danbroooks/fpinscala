package fpinscala.solutions.util.e4

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

  def map2[B,C](b: Option[B])(f: (A, B) => C): Option[C] =
    for { aa <- this; bb <- b } yield f(aa, bb)

  def filter(f: A => Boolean): Option[A] =
    flatMap(get => if (f(get)) Some(get) else None)
}

object Option {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List()): Option[List[B]])((hd, tl) => f(hd).map2(tl)(_ :: _))
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
