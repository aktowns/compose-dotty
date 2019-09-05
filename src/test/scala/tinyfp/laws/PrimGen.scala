import hedgehog._
import hedgehog.runner._

trait PrimGen[A]:
  def gen: Gen[A]
  def fn1: Gen[A => A]

object PrimGen:
  def apply[T] given PrimGen[T] = the[PrimGen[T]]

given as PrimGen[Int]:
  def gen: Gen[Int] = Gen.int(Range.linear(0, 100))
  def fn1: Gen[Int => Int] =
    Gen.int(Range.linear(-50, 50)).map(i => x => x + i)

given as PrimGen[String]:
  def gen: Gen[String] = Gen.string(Gen.alpha, Range.linear(0, 100))
  def fn1: Gen[String => String] = gen.map(s => ss => s + ss)