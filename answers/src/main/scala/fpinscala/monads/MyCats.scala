package fpinscala.monads

import cats.free.Free
import cats.free.Free.liftF

sealed trait KVStoreA[A]
case class Put[T](key: String, value: T) extends KVStoreA[Unit]
case class Get[T](key: String) extends KVStoreA[Option[T]]
case class Delete(key: String) extends KVStoreA[Unit]

/**
  * Created by j on 2/14/18.
  */
object MyCats {

  type KVStore[A] = Free[KVStoreA, A]


  // Put returns nothing (i.e. Unit).
  def put[T](key: String, value: T): KVStore[Unit] =
    liftF[KVStoreA, Unit](Put[T](key, value))

  // Get returns a T value.
  def get[T](key: String): KVStore[Option[T]] =
    liftF[KVStoreA, Option[T]](Get[T](key))

  // Delete returns nothing (i.e. Unit).
  def delete(key: String): KVStore[Unit] =
    liftF(Delete(key))

  // Update composes get and set, and returns nothing.
  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()

  def main(args: Array[String]): Unit = {
    def program: KVStore[Option[Int]] =
      for {
        _ <- {
            println("wild-cats put")
            put("wild-cats", 2)
          }
        _ <- update[Int]("wild-cats", (_ + 12))
        _ <- put("tame-cats", 5)
        n <- get[Int]("wild-cats")
        _ <- delete("tame-cats")
      } yield {
        println("yield: "+n)
        n
      }
    val tmp = program
    println("works!")

  }
}


