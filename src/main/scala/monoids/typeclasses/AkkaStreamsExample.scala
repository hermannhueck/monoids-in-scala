package monoids.typeclasses

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import monoids.typeclasses.minmax.{Max, Min}
import monoids.typeclasses.monoid.Monoid
import monoids.typeclasses.sumprod.Sum

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object AkkaStreamsExample extends App {

  implicit val system: ActorSystem = ActorSystem("AkkaStreamsExample")
  implicit val materializer: ActorMaterializer = ActorMaterializer.create(system) // ActorMaterializer()

  println("\n-----\n")

  implicit class MonoidIterable[T](val it: Source[T, NotUsed]) {
    def combineAll(implicit M: Monoid[T]): Future[T] = it.runFold(M.empty)(M.combine)
  }

  val input: Array[String] = "Textanalyse mit Monoiden und mit Iterables macht Spaß".split("""\s+""")

  try {
    task1(input)
    task2(input)
  } finally {
    val terminated = Await.result(system.terminate(), 3.seconds)
    println(terminated)
  }

  private def task1(input: Iterable[String]): Unit = {

    def expand(word: String): (Max, Min, Sum, Sum) = {
      (Max(word.length), Min(word.length), Sum(word.length), Sum(1))
    }

    val source: Source[String, NotUsed] =
      Source.fromIterator(() => input.iterator)
    val future: Future[(Max, Min, Sum, Sum)] =
      source.map(_.toLowerCase).map(expand).combineAll
    val (max, min, chars, words) =
      Await.result(future, 3.seconds)

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

    val source: Source[String, NotUsed] =
      Source.fromIterator(() => input.iterator)
    val future: Future[(Max, Min, Sum, Sum, Map[String, Sum])] =
      source.map(_.toLowerCase).map(expand).combineAll
    val (max, min, chars, words, wordOccurences) =
      Await.result(future, 3.seconds)

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
