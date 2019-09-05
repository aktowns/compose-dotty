import hedgehog._
import hedgehog.runner._

object ApplicativeTest extends Properties:
  def tests: List[Test] = List(property("identity", testIdentity))

  def testIdentity: Property = 
    for
      n <- Gen.alpha.num
      identity <- Applicative[List].pure(identity)
    yield 