import typeclass._
import data._

import given instance._
import given data._

case class Config (magic: Int)
type AppError[A] = Either[String, A]
type MyStack[A]  = ReaderT[Config, AppError, A]

given [A] as Monad[MyStack] = ReaderTMonad[Config, AppError]
given as Monad[AppError]    = EitherRightBiasedMonad[String]

def throwError[A](s: String): AppError[A] = Left(s)

def readValue(i: Int): MyStack[Int] = the[Monad[MyStack]].pure(3)

throwError("oh no").lift.lift.lift.lift.lift

val program = for
  y <- ReaderT.ask[Config, AppError]
  x <- readValue(1)
  z <- throwError("oh no").lift
yield y.magic + x

program.runReaderT(Config(39))