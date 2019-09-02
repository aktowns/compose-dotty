package parsec

import data._
import typeclass._

case class ParseError()

case class ParsecT[S, U, M[_], A](
  runParsecT: [B] =>> State[S, U] 
                  => (A => State[S, U] => ParseError => M[B]) // consumed ok
                  => (ParseError => M[B])                     // consumed err
                  => (A => State[S, U] => ParseError => M[B]) // empty ok
                  => (ParseError => M[B])                     // empty err
                  => M[B])