package aoc2023

import scala.jdk.CollectionConverters._

object Day2 {
  def main(args: Array[String]): Unit = {
    val input = new java.io.File("src/main/resources/input2.txt").toPath
    val lines = java.nio.file.Files.readAllLines(input).asScala.toSeq
    val games = lines.map(parseLine)
    val result1 = part1(games)
    println(result1)
    val result2 = part2(games)
    println(result2)
  }

  final case class Game(id: Int, cubes: Seq[(Int, Int, Int)])

  private val zeros = (0, 0, 0)

  private def parseLine(line: String): Game = {
    val gameAndCubes = line.split(':')
    val gameId = gameAndCubes(0).substring("Game ".length()).toInt
    val cubes = gameAndCubes(1)
      .split(';')
      .map { s =>
        val rgb = s.split(',').map(_.trim().split(' '))
        // println(s"\t$s: ${rgb.map(_.mkString("|")).mkString(";")}")
        def colorCount(color: String): Int =
          rgb.find(p => p(1) == color).map(_(0).toInt).getOrElse(0)
        (
          colorCount("red"),
          colorCount("green"),
          colorCount("blue")
        )
      }
    Game(gameId, cubes)
  }

  private def part1(games: Seq[Game]): Int = {
    val maxR = 12
    val maxG = 13
    val maxB = 14
    games.foldLeft[Int](0)((acc, game) => {
      val cubes = game.cubes
      if (cubes.exists(rgb => rgb._1 > maxR || rgb._2 > maxG || rgb._3 > maxB))
        acc
      else acc + game.id
    })
  }

  private def part2(games: Seq[Game]): Int = {
    def safeMax(cs: Seq[Int]): Int =
      cs.foldLeft(0)((max, r) => if (r > max) r else max)
    games.foldLeft[Int](0)((acc, game) => {
      val cubes = game.cubes
      val maxR = safeMax(cubes.map(_._1))
      val maxG = safeMax(cubes.map(_._2))
      val maxB = safeMax(cubes.map(_._3))
      acc + maxR * maxG * maxB
    })
  }
}
