package fpinscala.solutions.util.e4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = ???

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
