import scalaz.Monad

You can write `flatMap` using pattern matching.

def eitherMonad[E]: Monad[((type f[x] = Either[E, x]})#f] = {
    def unit[E, A](a: =>A):Either[E, A] = {
        Right(a)
    }

    def flatMap[A,B](fa: Either[E, A])(f: A=>Either[E, B]): Either[E,B] = {
        fa match{
            case Left(e) => {
                Left(e)
            }
            case Right(a) => {
                f(a)
            }
        }
    }
}

// ha ha!  got mostly right