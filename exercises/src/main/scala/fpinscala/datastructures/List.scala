package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = ???

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => 1+acc)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def leftProduct(l: List[Int])= foldLeft(l, 1)(_*_)

  def leftSum(l: List[Int]) = foldLeft(l, 0)(_+_)

  def leftLength(l: List[Int]) = foldLeft(l, 0)( (z, x) => 1+z)


  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())( (acc,h) => Cons(h, acc))

  def append_w_fold[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_,_))

  def addOne(l: List[Int]):List[Int] =
    foldRight(l, Nil:List[Int])((h, t)=>Cons(h+1, t))

  def doubleToS(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t)=>Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])( (h,t) => Cons(f(h),t))

  def filter[A](l: List[A])(f: A=>Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t)=> {
      if(f(h))
        Cons(h, t)
      else
        t
    })

  //List.flatMap(List(1,2,3,4,5))( i=>List(i,i) )
  def flatMap[A,B](as: List[A])(f: A=>List[B]): List[B] = {
    foldRight(as, Nil:List[B])((h,t)=>append(t, f(h)))
  }

  // List.filterViaFlatMap(List(1,2,3,4,5))( x=> x>2 )
  def filterViaFlatMap[A](as: List[A])(f: A=>Boolean): List[A] = {
    flatMap(as)(x => {
      if(f(x))
        List(x)
      else
        Nil
    })
  }

  def zipWith[A](l: List[A], r: List[A])(f: (A,A)=>A): List[A] ={
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
    }
  }

  def subseqence[A](l: List[A], r: List[A]): Boolean = {
    (l,r) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case(Cons(lh,lt), Cons(rh,rt)) => {
        if(lh == rh){
          subseqence(lt,rt)
        }else{
          subseqence(lt,Cons(rh,rt))
        }
      }
    }
  }
}
