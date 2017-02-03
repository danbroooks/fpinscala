package fpinscala.solutions.util.e5

import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) => cons(h(), t().take(n  - 1))
      case _ => this
    } else empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => {
      if (p(h)) cons(h, t)
      else empty
    })

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => (p(h)) && t)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B) =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean) =
    foldRight(empty[A])((h, t) => {
      if (f(h)) cons(h, t)
      else t
    })

  def append[B >: A](s: => Stream[B]) =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]) =
    foldRight(empty[B])((h, t) => {
      f(h).append(t)
    })
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    unfold(a)(x => Some((x, x)))

  def ones: Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def fibs: Stream[Int] =
    unfold((0, 1))(x => x match {
      case (curr, next) => Some((curr, (next, curr + next)))
    })

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((curr, next)) => cons(curr, unfold(next)(f))
    case None => empty
  }
}
