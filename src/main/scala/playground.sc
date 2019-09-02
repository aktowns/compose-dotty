import typeclass._
import data._

import given instance._
import given data._

case class Config (magic: Int)
case class AppState (bump: Int)

type AppError[A] = Either[String, A]
type ReaderStack[A]  = ReaderT[Config, AppError, A]
type MyStack[A] = StateT[AppState, ReaderStack, A]

given [A] as Monad[ReaderStack] = ReaderTMonad[Config, AppError]
given [A] as Monad[MyStack] = StateTMonad[AppState, ReaderStack]

def throwError[A](s: String): AppError[A] = Left(s)

def readValue(i: Int): MyStack[Int] = the[Monad[MyStack]].pure(3)

def increment(): MyStack[Unit] = 
  StateT.modify(s => AppState(s.bump + 1))

val program: MyStack[Int] = for
  //_ <- ReaderT.ask.lift
  y <- StateT.lift(ReaderT.ask[Config, AppError])

  initialState <- readValue(1)
  _ <- StateT.put[AppState, ReaderStack](AppState(initialState))

  _ <- increment()
  _ <- increment()
  _ <- increment()

  newState <- StateT.get[AppState, ReaderStack]
 // z <- StateT.lift(ReaderT.lift(throwError(s"oh no: $y")))
yield y.magic + newState.bump


program.runStateT(AppState(0)).runReaderT(Config(39))