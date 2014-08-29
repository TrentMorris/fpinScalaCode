import scala.{ Option => _, Either => _, _ }

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }
  def filter(f: A => Boolean): Option[A] = {
    this match {
      case None => None
      case Some(x) => if (f(x)) Some(x) else None
    }
  }
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x-m, 2))))
  }
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aV => b map (bV => f(aV,bV)))
  }
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    Some(for (ar <- a) yield {
        ar match {
          case None => return None
          case Some(x) => x } } )
  }
  
}

val x = Some(4)
val y = None
x.map(((x: Int) => Some(x + 2)))
x.flatMap(((x: Int) => Some(x + 2)))
val one = x.getOrElse(3)
val two = y.getOrElse(2)
val three = y.orElse(Some(3))
val four = x.orElse(Some(0))
val five = x.filter((x: Int) => x > 3)
val six = x.filter((x: Int) => x > 100)
val seven = x.variance(List(1.0,2.0,3.0, 4.2,43.2))
val eight = x.map2(Some(3), Some(4))((x: Int, y: Int) => x + y)
val nine = x.sequence(List(Some(3),Some(4),Some(2), None))