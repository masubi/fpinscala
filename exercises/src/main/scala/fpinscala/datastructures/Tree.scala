package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]



object Tree {

  def size[A](t: Tree[A]): Int ={
    t match{
      case Branch(l,r) => return 1+size(l)+size(r)
      case Leaf(v) => return 1
    }
  }

/*
  def maximum[Int](t: Tree[Int]): Int ={
    t match {
      case Branch(l,r) => return maximum(l).max(maximum(r))
      case Leaf(v) => v
    }
  }
*/

  def depth[A](t: Tree[A]): Int ={
    t match{
      case Branch(l,r) => return 1+(depth(l).max(depth(r)))
      case Leaf(v) => return 1
    }
  }

  def map[A,B](t: Tree[A])(f: A => B):Tree[B] ={
    t match{
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }

  val testTree = Branch(Branch(Leaf(1),Leaf(2)), Branch(Leaf(3),Leaf(4)))
}