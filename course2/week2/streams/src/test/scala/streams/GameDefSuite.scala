package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameDefSuite extends FunSuite {

  new GameDef with StringParserTerrain {
    override val level: String =
      """ST
        |oo
        |oo""".stripMargin

    test("isStanding") {
      assert(Block(Pos(0, 0), Pos(0, 0)).isStanding)
      assert(Block(Pos(2, 2), Pos(2, 2)).isStanding)

      assert(!Block(Pos(0, 0), Pos(0, 1)).isStanding)
      assert(!Block(Pos(0, 0), Pos(1, 0)).isStanding)
    }

    test("isLegal") {
      assert(Block(Pos(0,0), Pos(1,0)).isLegal)
      assert(Block(Pos(1,0), Pos(2,0)).isLegal)
      assert(Block(Pos(0,1), Pos(1,1)).isLegal)
      assert(Block(Pos(1,1), Pos(2,1)).isLegal)

      assert(!Block(Pos(2,1), Pos(3,1)).isLegal)
      assert(!Block(Pos(1,2), Pos(2,2)).isLegal)
    }
  }
}
