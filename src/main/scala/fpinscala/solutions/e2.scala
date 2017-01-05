package fpinscala.solutions

object e2 {
  def fib(n: Int) : Int = {

    @annotation.tailrec
    def fibonacci(n: Int, prev: Int, acc: Int): Int = {
      if (n == 0) prev
      else fibonacci(n - 1, acc, acc + prev)
    }

    fibonacci(n, 0, 1)
  }
}


