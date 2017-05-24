package fpinscala
package solutions
package laziness

sealed trait Stream[+A] {
  import Stream._

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
   * Exercise 5.1
   *
   * Write a function to convert a Stream to a List, which will force
   * its evaluation and let you look at it in the REPL. You can
   * convert to the regular List type in the standard library. You can
   * place this and other functions that operate on a Stream inside
   * the Stream trait.
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => List(h()) ++ t().toList
  }

  /**
   * Exercise 5.2
   *
   * Write the function take(n) for returning the first n elements of
   * a Stream.
   *
   * Exercise 5.13
   *
   * Use unfold to implement take.
   */
  def take(n: Int): Stream[A] =
    unfold((n, this))({
      case (1, Cons(h, t)) => Some((h(), (0, empty)))
      case (i, Cons(h, t)) if (i > 0) => Some((h(), (i - 1, t())))
      case _ => None
    })

  /**
   * Exercise 5.2
   *
   * Write the function drop(n) for skipping the first n elements of a
   * Stream.
   */
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  /**
   * Exercise 5.3
   *
   * Write the function takeWhile for returning all starting elements
   * of a Stream that match the given predicate.
   *
   * Exercise 5.5
   *
   * Use foldRight to implement takeWhile.
   *
   * Exercise 5.13
   *
   * Use unfold to implement takeWhile.
   */
  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this)({
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    })

  /**
   * Exercise 5.4
   *
   * Implement forAll, which checks that all elements in the Stream
   * match a given predicate.
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => (p(h)) && t)

  /**
   * Exercise 5.6
   *
   * Implement headOption using foldRight.
   */
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /**
   * Exercise 5.7
   *
   * Implement map using foldRight.
   *
   * Exercise 5.13
   *
   * Use unfold to implement map.
   */
  def map[B](f: A => B) =
    unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  /**
   * Exercise 5.7
   *
   * Implement filter using foldRight.
   */
  def filter(f: A => Boolean) =
    foldRight(empty[A])((h, t) => {
      if (f(h)) cons(h, t)
      else t
    })

  /**
   * Exercise 5.7
   *
   * Implement append using foldRight, it should be non-strict in its
   * argument.
   */
  def append[B >: A](s: => Stream[B]) =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]) =
    foldRight(empty[B])((h, t) => {
      f(h).append(t)
    })

  /**
   * Exercise 5.13
   *
   * Use unfold to implement zipWith.
   */
  def zipWith[B, C](s: Stream[B])(f: (=> A, => B) => C): Stream[C] =
    unfold((this, s))({
      case (Cons(a, as), Cons(b, bs)) => Some((f(a(), b()), (as(), bs())))
      case _ => None
    })

  /**
   * Exercise 5.13
   *
   * Use unfold to implement zipAll, a function that should continue
   * the traversal as long as one of the streams has more elements
   */
  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s))({
      case (Cons(a, as), Empty) => Some(((Some(a()), None), (as(), empty[B])))
      case (Empty, Cons(b, bs)) => Some(((None, Some(b())), (empty[A], bs())))
      case (Cons(a, as), Cons(b, bs)) => Some(((Some(a()), Some(b())), (as(), bs())))
      case _ => None
    })

  /**
   * Exercise 5.14
   *
   * Implement startsWith using functions you’ve written. It should
   * check if one Stream is a prefix of another. For instance,
   * Stream(1,2,3) startsWith Stream(1,2) would be true
   */
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile({
      case (_, Some(_)) => true
      case _ => false
    }).forAll({
     case (a, b) => a == b
    })
  }

  /**
   * Exercise 5.15
   *
   * Implement tails using unfold. For a given Stream, tails returns
   * the Stream of suffixes of the input sequence, starting with the
   * original Stream.
   */
  def tails: Stream[Stream[A]] =
    unfold(this)({
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _ => None
    }) append Stream(empty)

  /**
   * Exercise 5.16
   *
   * Generalize tails to the function scanRight, which is like a
   * foldRight that returns a stream of the intermediate results.
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.foldRight(empty[B])((next, curr) => cons(next.foldRight(z)(f), curr))
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

  /**
   * Exercise 5.8
   *
   * Generalize ones slightly to the function constant, which returns
   * an infinite Stream of a given value.
   *
   * Exercise 5.12
   *
   * Write constant & ones in terms of unfold.
   */
  def constant[A](a: A): Stream[A] =
    unfold(a)(x => Some((x, x)))

  def ones: Stream[Int] = constant(1)

  /**
   * Exercise 5.9
   *
   * Write a function that generates an infinite stream of integers,
   * starting from n, then n + 1, n + 2, and so on.
   *
   * Exercise 5.12
   *
   * Write from in terms of unfold.
   */
  def from(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  /**
   * Exercise 5.10
   *
   * Write a function fibs that generates the infinite stream of
   * Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   *
   * Exercise 5.12
   *
   * Write fibs in terms of unfold.
   */
  def fibs: Stream[Int] =
    unfold((0, 1))({
      case (curr, next) => Some((curr, (next, curr + next)))
    })

  /**
   * Exercise 5.11
   *
   * Write a more general stream-building function called unfold. It
   * takes an initial state, and a function for producing both the
   * next state and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((curr, next)) => cons(curr, unfold(next)(f))
    case None => empty
  }
}
