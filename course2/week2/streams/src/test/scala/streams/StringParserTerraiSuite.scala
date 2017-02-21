package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StringParserTerraiSuite extends FunSuite {

  test("terrain function") {
    new StringParserTerrain {
      override val level: String =
        """ST
          |oo
          |oo""".stripMargin

      assert(terrain(Pos(0, 0)))
      assert(terrain(Pos(1, 1)))
      assert(terrain(Pos(2, 1)))
      assert(!terrain(Pos(2, 2)))
      assert(!terrain(Pos(3, 0)))
    }
  }

  test("find char") {
    lazy val vector: String ⇒ Vector[Vector[Char]] = level ⇒
      Vector(level.split("\n").map(str => Vector(str: _*)): _*)

    new StringParserTerrain {
      override val level: String =
        """ST
          |oo
          |oo""".stripMargin

      val levelVector: Vector[Vector[Char]] = vector(level)
      assert(findChar('S', levelVector) == Pos(0, 0))
      assert(findChar('T', levelVector) == Pos(0, 1))
      assert(findChar('o', levelVector) == Pos(1, 0))
    }
  }
}
