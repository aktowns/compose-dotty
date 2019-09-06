package tinyfp.laws

import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Monoid

trait MonoidLaws[A: Monoid]:
  def gen: Gen[A]

  def tests: List[Test] = List(
    property("left identity", testLeftIdentity),
    property("right identity", testRightIdentity),
    property("associativity", testAssociativity)
  )

  def testLeftIdentity: Property = 
    for
      n <- gen.log("n")
    yield (Monoid[A].mempty <> n) ==== n

  def testRightIdentity: Property = 
    for
      n <- gen.log("n")
    yield (n <> Monoid[A].mempty) ==== n

  def testAssociativity: Property = 
    for 
      x <- gen.log("x")
      y <- gen.log("y")
      z <- gen.log("z")
    yield (x <> y) <> z ==== x <> (y <> z)

