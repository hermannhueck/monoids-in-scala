package monoids.typeclasses

import monoids.typeclasses.minmax.{Max, Maximum, Min, Minimum}
import monoids.typeclasses.monoid.Monoid
import monoids.typeclasses.sumprod.Sum

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object SparkExample extends App {

  println("\n----- Spark (not yet supported in Scala 2.12)")

    implicit class MonoidRDD[T](val rdd: RDD[T]) {
      def combineAll(implicit M: Monoid[T]): T = rdd.fold(M.empty)(M.combine)
    }

    val input = "Textanalyse mit Monoiden und mit Spark macht Spaß".split("""\s+""")

    val conf = new SparkConf().setAppName(getClass.getSimpleName).setMaster("local")
    val sparkContext: SparkContext = new SparkContext(conf)

    try {

      task1(input, sparkContext)
      task2(input, sparkContext)

    } finally {
      sparkContext.stop()
    }

    private def task1(input: Array[String], sparkContext: SparkContext): Unit = {

      def expand(word: String): (Max, Min, Sum, Sum) = {
        (Max(word.length), Min(word.length), Sum(word.length), Sum(1))
      }

      val (max, min, chars, words) =
        sparkContext.parallelize(input).map(_.toLowerCase).map(expand).combineAll

      println(
        s"""\nFinished calculation:
           |  - max word length: ${max.asInstanceOf[Maximum].value}
           |  - min word length: ${min.asInstanceOf[Minimum].value}
           |  - total characters: ${chars.value}
           |  - total words: ${words.value}
           |  - average word length: ${chars.value / words.value}
           |""".stripMargin)
    }

    private def task2(input: Array[String], sc: SparkContext): Unit = {

      def expand(word: String): (Max, Min, Sum, Sum, Map[String, Sum]) = {
        (Max(word.length), Min(word.length), Sum(word.length), Sum(1), Map(word -> Sum(1)))
      }

      val (max, min, chars, words, wordOccurences) =
        sc.parallelize(input).map(_.toLowerCase).map(expand).combineAll

      val occurences =
        wordOccurences
          .mapValues(_.value)
          .toList
          .map { case (word, occ) => word + ": " + occ }
          .sorted
          .mkString(", ")

      println(
        s"""\nFinished calculation:
           |  - max word length: ${max.asInstanceOf[Maximum].value}
           |  - min word length: ${min.asInstanceOf[Minimum].value}
           |  - total characters: ${chars.value}
           |  - total words: ${words.value}
           |  - average word length: ${chars.value / words.value}
           |  - word occurences: $occurences
           |""".stripMargin)
    }

  println("\n-----\n")
}
