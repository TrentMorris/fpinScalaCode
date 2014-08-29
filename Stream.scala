trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }
  def take(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
      case _ => Stream.empty
    }
    else Stream()
  }
  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) s
      else s match {
        case Cons(h, t) => go(t(), n - 1)
        case _ => Stream()
      }
    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) Stream.cons(h(), t().takeWhile(p)) else t().takeWhile(p)
    case _ => Stream.empty
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a,b) => if (p(a)) Stream.cons(a,b) else Stream.empty)
  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) ((a,b) => if (p(a)) Stream.cons(a,b) else Stream.empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => if (p(h())) t().forAll(p) else false
    case _ => true
  }

  def map[B](p: A => B): Stream[B] = foldRight(Stream.empty[B])((a,b) => Stream.cons(p(a),b))

  def startsWith[A](s: Stream[A]): Boolean = sys.error("todo")

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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => Stream.cons(h, unfold(s)(f))
    case None => Stream.empty
  }
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fib(): Stream[Int] = {
    def go(prev2: Int, prev1: Int):Stream[Int] = Stream.cons(prev2, go(prev1, (prev1 + prev2)))
    go(0,1)
}
}
val twos: Stream[Int] = Stream.cons(102, str)
val str: Stream[Int] = Stream.cons(117, threes)
val threes: Stream[Int] = Stream.cons(99, fours)
val fours: Stream[Int] = Stream.cons(107, Empty)
val toList = twos.toList
val take = twos.take(2)
val takeList = take.toList
val drop = twos.drop(2)
val dropList = drop.toList
val takewhile = twos.takeWhile((x: Int) => x > 5)
val takewhileList = takewhile.toList
val forAllBool = twos.forAll((x: Int) => x > 5)
val forAllBool2 = twos.forAll((x: Int) => x > 1)
val takewhilefold = twos.takeWhileFold((x: Int) => x > 0)
val takewhilefoldList = takewhilefold.toList
val mapList = twos.map((x: Int) => x.asInstanceOf[Char])
val filterList = twos.filter((x: Int) => x > 100)
