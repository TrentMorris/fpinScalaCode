case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

import scala.{ Option => _, Either => _, _ }

sealed trait Either[+E, +A] {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
  	this match {
  		case Right(x) => f(x)
  		case Left(x) => Left(x)
  	}
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
  	this match {
  		case Right(x) => Right(x)
  		case Left(_) => b
  	}
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
  	for (aa <- this; bb <- b) yield f(aa,bb)
  }
}
//EXERCISE 6: Implement versions of map , flatMap , orElse , and map2 on
//Either that operate on the Right value.

val x: Either[Int, String] = Right("shit")
val y: Either[Int, String] = Left(69)
val r: Either[Int, String] = x.flatMap(x => Right(x+ "balls"))
val s: Either[Int, String] = x.orElse(x)
val z: Either[Int, String] = y.orElse(x)