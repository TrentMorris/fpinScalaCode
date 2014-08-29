sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](l: Tree[A]):Int = l match {
      case Leaf(_) => 1
      case Branch(left,right) => 1 + size(left) + size(right)
    }
  def max[A](l: Tree[Int]): Int = l match {
    case Leaf(x) => x
    case Branch(l, r) => if (max(l) > max(r)) max(l) else max(r)
  }

  def map[A,B](l:Tree[A], f:A => B ): Tree[B] = l match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l,f), map(r,f))
  }
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
  def fold[A](t:Tree[A]): A

}


val res0 = Leaf(4)
val res1 = Leaf(3)
val branch1 = Branch(res0, res1)
val res2 = Leaf(2)
val res3 = Leaf(1)
val branch2 = Branch(res2, res3)
val branch3 = Branch(branch1, branch2)

// Tree.size(branch3)
// Tree.max(branch3)
// Tree.map(branch3, ((x:Int) => x * 4))
// Tree.depth(branch3)