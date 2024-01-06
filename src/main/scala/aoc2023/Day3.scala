package aoc2023

import scala.jdk.CollectionConverters._
import scala.annotation.tailrec

object Day3 {
  def main(args: Array[String]): Unit = {
    val result1 = part1("src/main/resources/input3.txt")
    println(result1)
    val result2 = part2("src/main/resources/input3.txt")
    println(result2)
  }

  def part1(path: String): Int = {
    val inputPath = new java.io.File(path).toPath
    val lines = java.nio.file.Files.readAllLines(inputPath).asScala.toArray
    val engineMap = lines.map(_.toCharArray)
    val lineCount = engineMap.length
    val lineLength = engineMap(0).length
    @tailrec def walkMap(accSum: Int, accNum: Int, line: Int, pos: Int): Int = {
      if (line >= lineCount) accSum
      else {
        val currChar = engineMap(line)(pos)
        val atDigitNow = currChar.isDigit
        val atLastChar = pos == lineLength - 1
        val (newLine, newPos) =
          if (atLastChar) (line + 1, 0) else (line, pos + 1)
        val isNumberComplete = accNum > 0 && (atLastChar || !atDigitNow)
        if (isNumberComplete) {
          val finalNum =
            if (atLastChar && atDigitNow) accNum * 10 + currChar.asDigit
            else accNum
          val n = finalNum.toString.length
          val topPos = pos - n - 1
          val topLine = line - 1
          val bottomPos = pos
          val bottomLine = line + 1
          val isPartOfEngine = checkPartOfEngine(
            engineMap,
            (topPos, topLine),
            (bottomPos, bottomLine)
          )
          val newAccSum = if (isPartOfEngine) accSum + finalNum else accSum
          walkMap(newAccSum, 0, newLine, newPos)
        } else if (atDigitNow) {
          if (atLastChar) { // hack for single-digit rightmost number
            val isPartOfEngine = checkPartOfEngine(
              engineMap,
              (pos - 1, line - 1),
              (pos + 1, line + 1)
            )
            val newAccSum =
              if (isPartOfEngine) accSum + currChar.asDigit else accSum
            walkMap(newAccSum, 0, newLine, newPos)
          } else {
            walkMap(accSum, accNum * 10 + currChar.asDigit, newLine, newPos)
          }
        } else {
          walkMap(accSum, 0, newLine, newPos)
        }
      }
    }
    walkMap(0, 0, 0, 0)
  }

  def part2(path: String): Long = {
    val inputPath = new java.io.File(path).toPath
    val lines = java.nio.file.Files.readAllLines(inputPath).asScala.toArray
    val engineMap = lines.map(_.toCharArray)
    val lineCount = engineMap.length
    val lineLength = engineMap(0).length
    @tailrec def walkMap(
        accGears: NumGearsMap,
        accNum: Int,
        line: Int,
        pos: Int
    ): NumGearsMap = {
      if (line >= lineCount) accGears
      else {
        val currChar = engineMap(line)(pos)
        val atDigitNow = currChar.isDigit
        val atLastChar = pos == lineLength - 1
        val (newLine, newPos) =
          if (atLastChar) (line + 1, 0) else (line, pos + 1)
        val isNumberComplete = accNum > 0 && (atLastChar || !atDigitNow)
        if (isNumberComplete) {
          val finalNum =
            if (atLastChar && atDigitNow) accNum * 10 + currChar.asDigit
            else accNum
          val n = finalNum.toString.length
          val topPos = pos - n - (if (atDigitNow) 0 else 1) // hack to correctly find left index
          val topLine = line - 1
          val bottomPos = pos
          val bottomLine = line + 1
          var adjacentStars =
            findAdjacentStars(engineMap, topPos, topLine, bottomPos, bottomLine)
          val newAccGears =
            accGears + (NumPoint(finalNum, line, pos) -> adjacentStars)
          walkMap(newAccGears, 0, newLine, newPos)
        } else if (atDigitNow) {
          if (atLastChar) { // hack for single-digit rightmost number
            var adjacentStars = findAdjacentStars(
              engineMap,
              pos - 1,
              line - 1,
              pos + 1,
              line + 1
            )
            val newAccGears = accGears + (NumPoint(currChar.asDigit, line, pos) -> adjacentStars)
            walkMap(newAccGears, 0, newLine, newPos)
          } else {
            walkMap(accGears, accNum * 10 + currChar.asDigit, newLine, newPos)
          }
        } else {
          walkMap(accGears, 0, newLine, newPos)
        }
      }
    }
    val gearsMap = walkMap(Map.empty[NumPoint, Seq[GearPoint]], 0, 0, 0)
    gearsMap
      .toSeq
      .flatMap { case (k, v) =>
        v.map(gp => gp -> k)
      }
      .groupBy(_._1)
      .collect(_ match {
        case (gear, Seq(p1, p2)) =>
          (p1._2.num * p2._2.num).toLong
      })
      .foldLeft(0L)((acc, gearNumber) => acc + gearNumber)
  }

  private final case class NumPoint(num: Int, line: Int, pos: Int) {
    override def toString(): String = s"[$num]($line, $pos)"
  }

  private final case class GearPoint(line: Int, pos: Int) {
    override def toString(): String = s"[*]($line, $pos)"
  }

  private type NumGearsMap = Map[NumPoint, Seq[GearPoint]]

  private val notSymbols = Set('.') ++ ('0' to '9')

  private def checkPartOfEngine(
      engineMap: Array[Array[Char]],
      topLeft: (Int, Int),
      bottomRight: (Int, Int)
  ): Boolean = {
    val (topPos, topLine) = topLeft
    val (bottomPos, bottomLine) = bottomRight
    val lineCount = engineMap.length
    val lineLength = engineMap(0).length
    val left = math.max(0, topPos)
    val right = math.min(bottomPos, lineLength - 1)
    val isAdjTop = topLine >= 0 && {
      (engineMap(topLine)
        .slice(left, right + 1)
        .toSet -- notSymbols).nonEmpty
    }
    val isAdjBottom = bottomLine < lineCount && {
      (engineMap(bottomLine)
        .slice(left, right + 1)
        .toSet -- notSymbols).nonEmpty
    }
    val isAdjLeft = left > 0 && {
      engineMap(topLine + 1)(left) != '.'
    }
    val isAdjRight = right < lineLength && {
      engineMap(topLine + 1)(right) != '.'
    }
    isAdjTop || isAdjBottom || isAdjLeft || isAdjRight
  }

  private def findAdjacentStars(
      engineMap: Array[Array[Char]],
      topPos: Int,
      topLine: Int,
      bottomPos: Int,
      bottomLine: Int
  ): Seq[GearPoint] = {
    val lineCount = engineMap.length
    val lineLength = engineMap(0).length
    val left = math.max(0, topPos)
    val right = math.min(bottomPos, lineLength - 1)
    val adjTop =
      if (topLine < 0) Seq.empty[GearPoint]
      else {
        engineMap(topLine)
          .slice(left, right + 1)
          .zipWithIndex
          .collect {
            case (ch, pos) if ch == '*' => GearPoint(topLine, left + pos)
          }
          .toSeq
      }
    val adjBottom =
      if (bottomLine >= lineCount) Seq.empty[GearPoint]
      else {
        engineMap(bottomLine)
          .slice(left, right + 1)
          .zipWithIndex
          .collect {
            case (ch, pos) if ch == '*' => GearPoint(bottomLine, left + pos)
          }
          .toSeq
      }
    val adjLeft =
      if (left < 0) Seq.empty[GearPoint]
      else {
        if (engineMap(topLine + 1)(left) == '*')
          Seq(GearPoint(topLine + 1, left))
        else Seq.empty[GearPoint]
      }
    val adjRight =
      if (right >= lineLength) Seq.empty[GearPoint]
      else {
        if (engineMap(topLine + 1)(right) == '*')
          Seq(GearPoint(topLine + 1, right))
        else Seq.empty[GearPoint]
      }
    adjTop ++ adjBottom ++ adjLeft ++ adjRight
  }
}
