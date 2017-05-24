package fpinscala
package solutions
package functional_data_structures

import annotation.tailrec

object Exercises {

  import List._

  /**
   * Exercise 3.1
   *
   * What will be the result of the following match expression?
   */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  object ListExercises {

    /**
     * Exercise 3.2
     *
     * Implement the function tail for removing the first element of a
     * List. Note that the function takes constant time. What are
     * different choices you could make in your implementation if the
     * List is Nil?
     */
    def tail[A](ls: List[A]): List[A] = ls match {
      case Nil => sys.error("Cannot apply to empty list")
      case Cons(_, tl) => tl
    }

    /**
     * Exercise 3.3
     *
     * Using the same idea, implement the function setHead for
     * replacing the first element of a List with a different value.
     */
    def setHead[A](hd: A, ls: List[A]): List[A] = ls match {
      case Nil => sys.error("setCannot apply to empty list")
      case Cons(_, tl) => Cons(hd, tl)
    }

    /**
     * Exercise 3.4
     *
     * Generalize tail to the function drop, which removes the first n
     * elements from a list. Note that this function takes time
     * proportional only to the number of elements being dropped—we
     * don’t need to make a copy of the entire List.
     */
    def drop[A](ls: List[A], n: Int): List[A] =
      if (n <= 0) ls
      else ls match {
        case Nil => Nil
        case Cons(_, tl) => drop(tl, n - 1)
      }

    /**
     * Exercise 3.5
     *
     * Implement dropWhile, which removes elements from the List
     * prefix as long as they match a predicate.
     */
    def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
      case Nil => Nil
      case Cons(hd, tl) => if (f(hd)) dropWhile(tl, f) else ls
    }

    /**
     * Exercise 3.6
     *
     * Not everything works out so nicely. Implement a function, init,
     * that returns a List consisting of all but the last element of a
     * List. So, given List(1,2,3,4), init will return List(1,2,3).
     * Why can’t this function be implemented in constant time like
     * tail?
     */
    def init[A](ls: List[A]): List[A] = ls match {
      case Nil => sys.error("Cannot apply to empty list")
      case Cons(_, Nil) => Nil
      case Cons(hd, tl) => Cons(hd, init(tl))
    }

    /**
     * Exercise 3.7
     *
     * Can product, implemented using foldRight, immediately halt the
     * recursion and return 0.0 if it encounters a 0.0? Why or why
     * not? Consider how any short-circuiting might work if you call
     * foldRight with a large list.
     */
    // --

    /**
     * Exercise 3.8
     *
     * See what happens when you pass Nil and Cons themselves to
     * foldRight, like this:
     *
     * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10
     *
     * What do you think this says about the relationship between
     * foldRight and the data constructors of List?
     */
    // --

    /**
     * Exercise 3.9
     *
     * Compute the length of a list using foldRight.
     */
    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, n) => n + 1)

    /**
     * Exercise 3.10
     *
     * Our implementation of foldRight is not tail-recursive and will
     * result in a StackOver- flowError for large lists (we say it’s
     * not stack-safe). Convince yourself that this is the case, and
     * then write another general list-recursion function, foldLeft,
     * that is tail-recursive.
     */
    @tailrec def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    /**
     * Exercise 3.11
     *
     * Write sum using foldLeft.
     */
    def sumLeft(ns: List[Int]): Int =
      foldLeft(ns, 0)(_ + _)

    /**
     * Exercise 3.11
     *
     * Write product using foldLeft.
     */
    def productLeft(ns: List[Int]): Double =
      foldLeft(ns, 1.0)(_ * _)

    /**
     * Exercise 3.11
     *
     * Write a function to compute the length of a list using
     * foldLeft.
     */
    def lengthLeft(ns: List[Int]): Int =
      foldLeft(ns, 0)((n, _) => n + 1)

    /**
     * Exercise 3.12
     *
     * Write a function that returns the reverse of a list (given
     * List(1,2,3) it returns List(3,2,1)). See if you can write it
     * using a fold.
     */
    def reverse[A](ns: List[A]): List[A] =
      foldLeft(ns, Nil: List[A])((xs, x) => Cons(x, xs))

    /**
     * Exercise 3.13
     *
     * Can you write foldLeft in terms of foldRight? How about the
     * other way around? Implementing foldRight via foldLeft is useful
     * because it lets us implement foldRight tail-recursively, which
     * means it works even for large lists without overflowing the
     * stack.
     */
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(as), z)((b, a) => f(a, b))
    }

    /**
     * Exercise 3.14
     *
     * Implement append in terms of either foldLeft or foldRight.
     */
    def append[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)(Cons(_, _))

    /**
     * Exercise 3.15
     *
     * Write a function that concatenates a list of lists into a
     * single list. Its runtime should be linear in the total length
     * of all lists. Try to use functions we have already defined.
     */
    def flatten[A](lists: List[List[A]]): List[A] =
      foldRight(lists, List[A]())(append)

    /**
     * Exercise 3.16
     *
     * Write a function that transforms a list of integers by adding 1
     * to each element.
     */
    def increment(numbers: List[Int]): List[Int] =
      map(numbers)(_ + 1)

    /**
     * Exercise 3.17
     *
     * Write a function that turns each value in a List[Double] into a
     * String
     */
    def doublesToStrings(dubs: List[Double]): List[String] =
      map(dubs)(_.toString)

    /**
     * Exercise 3.18
     *
     * Write a function map that generalizes modifying each element in
     * a list while maintaining the structure of the list
     */
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, List[B]())((x, xs) => Cons(f(x), xs))

    /**
     * Exercise 3.20
     *
     * Write a function flatMap that works like map except that the
     * function given will return a list instead of a single result,
     * and that list should be inserted into the final resulting list.
     */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      flatten(map(as)(f))

    /**
     * Exercise 3.19
     *
     * Write a function filter that removes elements from a list
     * unless they satisfy a given predicate. Use it to remove all odd
     * numbers from a List[Int].
     *
     * Exercise 3.21
     *
     * Use flatMap to implement filter.
     */
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) List(a) else Nil)

    /**
     * Exercise 3.22
     *
     * Write a function that accepts two lists and constructs a new
     * list by adding corresponding elements.
     */
    def combine(as: List[Int], bs: List[Int]): List[Int] =
      zipWith(as, bs)((a: Int, b: Int) => a + b)

    /**
     * Exercise 3.23
     *
     * Generalize the function you just wrote so that it’s not
     * specific to integers or addition.
     */
    def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
      case (Nil, as) => as
      case (as, Nil) => as
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

    /**
     * Exercise 3.24
     *
     * Implement hasSubsequence for checking whether a List contains
     * another List as a subsequence. For instance, List(1,2,3,4)
     * would have List(1,2), List(2,3), and List(4) as subsequences,
     * among others.
     */
    @tailrec def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      if (startsWith(sup, sub)) true
      else sup match {
        case Nil => false
        case Cons(_, xs) => hasSubsequence(xs, sub)
      }
    }
  }

  object TreeExercises {

    /**
     * Exercise 3.25
     *
     * Write a function size that counts the number of nodes (leaves
     * and branches) in a tree.
     */
    def size[T](t: Tree[T]): Int =
      fold(t)(_ => 1)((l, r) => 1 + l + r)

    /**
     * Exercise 3.26
     *
     * Write a function maximum that returns the maximum element in a
     * Tree[Int]
     *
     * Exercise 3.29
     *
     * Generalize in terms of fold
     */
    def maximum(t: Tree[Int]): Int =
      fold(t)(i => i)(_ max _)

    /**
     * Exercise 3.27
     *
     * Write a function depth that returns the maximum path length
     * from the root of a tree to any leaf.
     *
     * Exercise 3.29
     *
     * Generalize in terms of fold
     */
    def depth[T](t: Tree[T]): Int =
      fold(t)(_ => 0)((l, r) => 1 + (l max r))

    /**
     * Exercise 3.28
     *
     * Write a function map, analogous to the method of the same name
     * on List, that modifies each element in a tree with a given
     * function.
     *
     * Exercise 3.29
     *
     * Generalize in terms of fold
     */
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

    /**
     * Exercise 3.29
     *
     * Generalize size, maximum, depth, and map, writing a new
     * function fold that abstracts over their similarities.
     * Reimplement them in terms of this more general function.
     */
    def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
      case Leaf(value) => l(value)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }
  }
}
