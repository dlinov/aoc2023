package aoc2023

import scala.jdk.CollectionConverters._
import scala.annotation.tailrec

object Day1 {
  def main(args: Array[String]): Unit = {
    val input = new java.io.File("src/main/resources/input1.txt").toPath
    val lines = java.nio.file.Files.readAllLines(input).asScala.toSeq
    val task1Result = task1(lines)
    println(task1Result)
    val task2Result = task2(lines)
    println(task2Result)
  }

  private def task1(lines: Seq[String]): Int = {
    lines.map(readNumber).sum
  }

  private def task2(lines: Seq[String]): Int = {
    lines.map(readSmartNumber).sum
  }

  private def readNumber(line: String): Int = {
    @tailrec def readFirstDigit(i: Int): Int = {
      if (line.charAt(i).isDigit) line.charAt(i).intValue - 48
      else readFirstDigit(i + 1)
    }

    @tailrec def readLastDigit(i: Int): Int = {
      if (line.charAt(i).isDigit) line.charAt(i).intValue - 48
      else readLastDigit(i - 1)
    }

    readFirstDigit(0) * 10 + readLastDigit(line.length - 1)
  }

  private def readSmartNumber(line: String): Int = {
    @tailrec def readFirstDigit(i: Int): Int = line match {
      case x if x.charAt(i).isDigit      => x.charAt(i).intValue - 48
      case x if x.startsWith("one", i)   => 1
      case x if x.startsWith("two", i)   => 2
      case x if x.startsWith("three", i) => 3
      case x if x.startsWith("four", i)  => 4
      case x if x.startsWith("five", i)  => 5
      case x if x.startsWith("six", i)   => 6
      case x if x.startsWith("seven", i) => 7
      case x if x.startsWith("eight", i) => 8
      case x if x.startsWith("nine", i)  => 9
      case _                             => readFirstDigit(i + 1)
    }

    @tailrec def readLastDigit(i: Int): Int = line match {
      case x if x.charAt(i).isDigit => x.charAt(i).intValue - 48
      case x if x.startsWith("one", i - "one".length() + 1)     => 1
      case x if x.startsWith("two", i - "two".length() + 1)     => 2
      case x if x.startsWith("three", i - "three".length() + 1) => 3
      case x if x.startsWith("four", i - "four".length() + 1)   => 4
      case x if x.startsWith("five", i - "five".length() + 1)   => 5
      case x if x.startsWith("six", i - "six".length() + 1)     => 6
      case x if x.startsWith("seven", i - "seven".length() + 1) => 7
      case x if x.startsWith("eight", i - "eight".length() + 1) => 8
      case x if x.startsWith("nine", i - "nine".length() + 1)   => 9
      case _ => readLastDigit(i - 1)
    }

    readFirstDigit(0) * 10 + readLastDigit(line.length - 1)
  }
}
