sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

val valid = new Validation[E, A]{
  def unit[A](a: =>A)=Success(a)

  def map2[A,B,C,E](fa: Validation[E, A], fb: Validation[E,B])(fab: (A,B) => C): Validation[E,C] ={
    (fa, fb) match {
      case (Success(a), Success(b)) => try {
        Success(fab(a,b))
      }catch{
        case e: Exception => Failure(e)
      }
      case (Failure(ah, at), Failure(bh, bt)) => {
        Failure(ah, Vector(bh) ++ at++bt)
      }
    }
  }
}