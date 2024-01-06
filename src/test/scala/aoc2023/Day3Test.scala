package aoc2023

object Day3Test {
  @main def test: Unit = {
    println("=== PART 1 TESTS ===")
    Seq(
      ("src/test/resources/day03/test1.txt", 4361),
      ("src/test/resources/day03/test2.txt", 413),
      ("src/test/resources/day03/test3.txt", 925),
      ("src/test/resources/day03/test4.txt", 62),
      ("src/test/resources/day03/test5.txt", 587),
      ("src/test/resources/day03/test6.txt", 423),
      ("src/test/resources/day03/test7.txt", 963 + 649 + 663),
    ).foreach((path, expected) => {
      val actual = Day3.part1(path)
      println(s"Actual: $actual\tExpected: $expected")
      assert(actual == expected)
    })

    println("=== PART 2 TESTS ===")
    Seq(
      ("src/test/resources/day03/test1.txt", 467835),
      ("src/test/resources/day03/test2.txt", 6756),
      ("src/test/resources/day03/test3.txt", 6756),
      ("src/test/resources/day03/test4.txt", 478),
      ("src/test/resources/day03/test5.txt", 1170),
      ("src/test/resources/day03/test6.txt", 2),
      ("src/test/resources/day03/test7.txt", 963*649),
    ).foreach((path, expected) => {
      val actual = Day3.part2(path)
      println(s"Actual: $actual\tExpected: $expected")
      assert(actual == expected)
    })
  }
}
