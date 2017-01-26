package fpinscala.solutions.util.e4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { aa <- this; bb <- b } yield f(aa, bb)
}

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
    as.foldRight(Right(List()): Either[E, List[B]])((hd, tl) => f(hd).map2(tl)(_ :: _))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
