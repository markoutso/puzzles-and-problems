package puzzles.bullsandcows

object BullsAndCows extends App {

  type Entry = Vector[Int]
  type Result = (Int, Int)
  type Solution = Seq[(Entry, Result)]

  object Game {
    val all: Seq[Game] = {
      for {
        c1 <- 1 to 4
        c2 <- 1 to 4
        c3 <- 1 to 4
        c4 <- 1 to 4
      } yield new Game(Vector(c1, c2, c3, c4))
    }
  }

  class Game(val choice: Entry) {

    private def remove(x: Int, xs: List[Int]): List[Int] = {
      xs match {
        case Nil => Nil
        case `x` :: rest => rest
        case first :: rest => first :: remove(x, rest)
      }
    }

    private def pairs(xs: List[Int], ys: List[Int]): Int = {
      xs match {
        case x :: rest if ys.contains(x) => 1 + pairs(rest, remove(x, ys))
        case _ :: rest => pairs(rest, ys)
        case _ => 0
      }
    }

    def check(guess: Entry): Result = {
      val bullPositions = for (i <- 0 until 4 if guess(i) == choice(i)) yield i
      val positionsLeft = List(0, 1, 2, 3).diff(bullPositions)
      val restGuessValues = positionsLeft.map(guess)
      val restSolutionValues = positionsLeft.map(choice)
      val nrOfCows = pairs(restGuessValues, restSolutionValues)
      (bullPositions.size, nrOfCows)
    }

    def solve(): Solution = {
      def loop(remaining: Seq[Game], acc: Solution): Solution = {
        if (remaining.isEmpty) acc
        else {
          val guess = remaining.head.choice
          val result = check(guess)
          val current = (guess, result)
          if (result == (4, 0)) acc :+ current
          else loop(remaining.tail.filter(_.check(guess) == result), acc :+ current)
        }
      }
      loop(Game.all, Seq())
    }
  }

  val solutions = Game.all.map(_.solve())

  // Game.solve() should produce a solution
  for ((g, s) <- Game.all.zip(solutions)) {
    assert(g.choice == s.last._1)
  }

  val max = solutions.maxBy(_.length)
  println(s"Max number of rounds for all games: ${max.length}")
}