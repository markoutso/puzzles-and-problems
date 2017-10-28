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

    def check(guess: Entry): Result = {
      val bullPositions = (for (i <- 0 until 4 if guess(i) == choice(i)) yield i).toSet
      val positionsLeft = Set(0, 1, 2, 3).diff(bullPositions)
      val restGuessValues = positionsLeft.map(guess)
      val restSolutionValues = positionsLeft.map(choice)
      val nrOfCows = restGuessValues.intersect(restSolutionValues)
      (bullPositions.size, nrOfCows.size)
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