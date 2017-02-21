package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait SampleLevel extends SolutionChecker {
    val level =
      """oSoo------
        |oToo------
        |-oooo-ooo-
        |-oooo-oooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  test("SampleLevel solution") {
    new SampleLevel {
      val expected = List(Right, Down, Left)
      assertResult(expected)(solution)
    }
  }

  test("SampleLevel from") {
    new SampleLevel {
      val expected = Stream()
      val took = from(Stream((Block(Pos(0, 1), Pos(0, 1)), List.empty)), Set.empty).take(100)

      for {
        (_, moves) ← took
      } println(s"size=${moves.size}")
      println(took)
      //just for print
      assert(true)
    }
  }

  test("SampleLevel pathsToGoal") {
    new SampleLevel {
      val expected = Stream(
        (Block(Pos(1,1),Pos(1,1)),List(Right, Down, Left))
      )

      assertResult(expected)(pathsToGoal)
    }
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

  test("done") {
    new Level1 {
      assert(done(Block(Pos(4,7), Pos(4,7))))
      assert(!done(Block(Pos(3,7), Pos(4,7))))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val block = Block(Pos(1,1), Pos(1,1))
      val emptyHistory: List[Move] = List(Up, Left)

      val neighs = Stream(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Up, Left)),
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Up, Left))
      )
      assertResult(neighs)(neighborsWithHistory(block, emptyHistory))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      val exploredBlocks = Set(Block(Pos(1, 2), Pos(1, 3)))

      val neighs = Stream(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Up, Left)),
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Up, Left))
      )

      assertResult(Stream((Block(Pos(2, 1), Pos(3, 1)), List(Down, Up, Left))))(newNeighborsOnly(neighs, exploredBlocks))
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
