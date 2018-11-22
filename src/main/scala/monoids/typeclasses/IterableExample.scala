package monoids.typeclasses

import monoids.typeclasses.minmax.{Max, Min}
import monoids.typeclasses.monoid.Monoid
import monoids.typeclasses.sumprod.Sum

object IterableExample extends App {

  println("\n-----\n")

  implicit class MonoidIterable[T](val it: Iterable[T]) {
    def combineAll(implicit M: Monoid[T]): T = it.fold(M.empty)(M.combine)
  }

  val input: Array[String] = "Textanalyse mit Monoiden und mit Iterables macht Spaß".split("""\s+""")

  task1(input)
  task2(input)

  private def task1(input: Iterable[String]): Unit = {

    def expand(word: String): (Max, Min, Sum, Sum) = {
      (Max(word.length), Min(word.length), Sum(word.length), Sum(1))
    }

    val (max, min, chars, words) =
      input.map(_.toLowerCase).map(expand).combineAll

    println(
      s"""Finished calculation:
         |  - max word length: $max
         |  - min word length: $min
         |  - total characters: ${chars.value}
         |  - total words: ${words.value}
         |  - average word length: ${chars.value / words.value}
         |""".stripMargin)
  }

  private def task2(input: Iterable[String]): Unit = {

    def expand(word: String): (Max, Min, Sum, Sum, Map[String, Sum]) = {
      (Max(word.length), Min(word.length), Sum(word.length), Sum(1), Map(word -> Sum(1)))
    }

    val (max, min, chars, words, wordOccurences) =
      input.map(_.toLowerCase).map(expand).combineAll

    val occurences =
      wordOccurences
        .mapValues(_.value)
        .toList
        .map { case (word, occ) => word + ": " + occ }
        .sorted
        .mkString(", ")

    println(
      s"""Finished calculation:
         |  - max word length: $max
         |  - min word length: $min
         |  - total characters: ${chars.value}
         |  - total words: ${words.value}
         |  - average word length: ${chars.value / words.value}
         |  - word occurences: $occurences
         |""".stripMargin)
  }

  println("-----\n")
}
