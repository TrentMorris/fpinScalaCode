/*
def fib(n: Int): Int = {
  @annotation.tailrec
  def acc(previous: Int, secondPrevious: Int, s: Int, current: Int): Int = {
    if (current == s) {
      previous + secondPrevious
    } else {
      acc((previous + secondPrevious), previous, s, current + 1)
    }
  }
  acc(0, 1, n, 2)
}

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  def loop(n: Int): Boolean = {
    if (n >= as.length - 1) true
    else if (gt(as(n), as(n + 1))) loop(n + 1)
    else false
  }
  loop(0)
}

def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
  (b: B) => f(a, b)
}

def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  (a: A) => f(a, _)
}

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  (a: A) => f(g(a))
}
*/
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
 @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def tail[A](ds: List[A]): List[A] = ds match {
  	case Nil => Nil
  	case Cons(x,xs) => xs
  }
  def setHead[A](d: A, ds:List[A]):List[A] = ds match {
  	case Nil => Cons(d,Nil)
  	case Cons(x,xs) => Cons(d,xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  def drop[A](l: List[A], n: Int): List[A] = { 
  	if (n==0) return l
  	l match {
  		case Nil => Nil
  		case Cons(x,xy) => drop(xy, n-1)
    }
  }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x,xy) => if (f(x)) return xy else dropWhile(xy, f)
    }
  }
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("idiot")
      case Cons(_, Nil) => Nil
      case Cons(x,y) => Cons(x,init(y))
    }
  }
 def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()) ((acc,h) => Cons(h,acc))
 def append[A](l:List[A], la:A):List[A] = foldLeft(l, List[A](la))((acc,h) => Cons(h,acc))
 def addOne[A](l:List[Int]): List[Int] = {
  l match {
    case Nil => Nil
    case Cons(x,xs) => Cons((x+1),addOne(xs))
    }
  }
  def dubToString[Double](l:List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case Cons(x,xs) => Cons((x.toString),dubToString(xs))
    }
  }
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x,xs) => Cons((f(x)), map(xs)(f))
    }
  }
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x,xy) => if (f(x)) Cons(x, filter(xy,f )) else filter(xy, f)
  }
}
// List(2,3,5,3,6,4,3,1).foldRight(0)((b,a) => a + 1)
//List(2,3,5,3,6,4,3,1).foldLeft(0)((b,a) => b + 1)
val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Cons(2,Nil))
val ex3: List[String] = Cons("a", Cons("b", Nil))
val ex4: List[Double] = Cons(1.0, Cons(2.0,Cons(3.0,Cons(4.0,Nil))))
val one = List.filter(ex4, ((x:Int) => (x < 2.1)))
//val toStrin = (x:Double) => x.toString 
//val one = List.map(ex4)((x:Double) => toStrin(x))
//val one = List.dubToString(ex4) -- dubToString --
//val one = List.addOne(ex2)      -- addOne -- 
//val one = List.append(ex2,3)      -- append --
//val two = List.append(ex3,"bish")
//val one = List.reverse(ex1)    --reverse--
//val two = List.reverse(ex2)
//val three = List.reverse(ex3)
//val twelve = List.init(ex1)    -- init --
//val thirteen = List.init(ex2)
//val fourteen = List.init(ex3)
//val one = List.tail(ex3) 					-- tail -- 
//val two = List.tail(ex2)
//val three = List.tail(ex1)
//val four = List.setHead(2.0,ex1)			-- setHead --
//val five = List.setHead("z",ex3)
//val six = List.setHead(4,ex2)
//val seven = List.drop(ex3,1)				-- drop --
//val eight = List.drop(ex3,2)
//val nine = List.drop(ex2,0)
//val ten = List.dropWhile(ex2, ((x:Int) => x > 1))     --dropWhile
//val eleven = List.dropWhile(ex3, ((x:String) => "a"==x))