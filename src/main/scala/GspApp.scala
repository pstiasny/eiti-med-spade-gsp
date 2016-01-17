import java.util.concurrent.TimeUnit

import com.google.common.base.Stopwatch
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.slf4j.LoggerFactory

import scala.collection.AbstractIterator
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

object sparkfixtures {


  implicit class Iterable2RddLike[T](iterable: Iterable[T]) {
    def count(): Long = iterable.size

    def distinct(): Iterable[T] = iterable.toSet

    def toLocalIterator: Iterator[T] = iterable.iterator
  }

  implicit class Iterator2RddLike[T](iterator: Iterator[T]) {
    def cache(): Iterable[T] = iterator.toList
  }

}

object model {

  case class Support(relativeSupport: Double)

  implicit def supportToNumber(support: Support): Double = support.relativeSupport

  object Support {
    def apply(supportedCount: Number, databaseSize: Number): Support = Support(supportedCount.longValue().toDouble / databaseSize.longValue())
  }

  case class Item(id: Int) {
    override def toString = s"$id"
  }

  implicit def idToItem(id: Int): Item = Item(id)

  case class Transaction(items: Set[Item], time: Long) {
    def contains(itemToFind: Item): Boolean = items.contains(itemToFind)

    def containsAll(itemSet: Set[Item]): Boolean = itemSet.forall(contains)
  }

  implicit val transactionOrdering = Ordering.by[Transaction, Long](_.time)

  case class DataSequence(transactions: Iterable[Transaction]) {
    def supports(pattern: SequentialPattern): Boolean = {
      val transactionIterator = transactions.iterator
      pattern.itemSets.forall { itemSet =>
        transactionIterator.exists(_.containsAll(itemSet))
      }
    }
  }

  sealed trait Data {
    def size: Long

    def frequentItems(minSupport: Support): Iterable[SequentialPattern]

    def countSupporting(pattern: SequentialPattern): Long
  }

  case class DataRdd(sequences: RDD[DataSequence]) extends Data {
    val size = sequences.count()

    def frequentItems(minSupport: Support): Iterable[SequentialPattern] = {
      val transactions = sequences.flatMap(_.transactions)
      val items = transactions.flatMap(_.items).distinct().toLocalIterator.toSet
      for {
        item <- items
        support = Support(transactions.filter(_.contains(item)).count(), size)
        if support >= minSupport
      } yield SequentialPattern(List(Set(item)))
    }

    override def countSupporting(pattern: SequentialPattern): Long = sequences.filter(_.supports(pattern)).count()
  }

  case class DataCollection(sequences: Iterable[DataSequence]) extends Data {

    import sparkfixtures._

    val size = sequences.count()

    def frequentItems(minSupport: Support): Iterable[SequentialPattern] = {
      val transactions = sequences.flatMap(_.transactions)
      val items = transactions.flatMap(_.items).distinct().toLocalIterator.toSet
      for {
        item <- items
        support = Support(transactions.filter(_.contains(item)).count(), size)
        if support >= minSupport
      } yield SequentialPattern(List(Set(item)))
    }

    override def countSupporting(pattern: SequentialPattern): Long = sequences.filter(_.supports(pattern)).count()
  }

  case class SequentialPattern(itemSets: List[Set[Item]]) {
    def support(data: Data): Support = {
      val result = Support(data.countSupporting(this), data.size)
      //println(s"Support of $itemSets is: $result")
      result
    }
  }

}

object Gsp {

  import model._

  def mineGsp(data: Data, minSupport: Support): Iterator[SequentialPattern] = {
    val generator: Iterator[List[SequentialPattern]] = new AbstractIterator[List[SequentialPattern]] {
      private var k = 1
      private var lastPatterns = data.frequentItems(minSupport)

      override def hasNext: Boolean = true

      def combinePatterns(left: SequentialPattern, right: SequentialPattern): Option[SequentialPattern] = {
        val leftHead :: leftMiddle = left.itemSets
        val rightTail :: rightMiddle = right.itemSets.reverse
        if (leftMiddle == rightMiddle) Some(SequentialPattern(leftHead +: leftMiddle :+ rightTail)) else None
      }

      def generateCandidates(candidateLength: Int, patternsToExpand: Iterable[SequentialPattern]): Iterable[SequentialPattern] = {
        for {
          (leftPattern, leftIndex) <- patternsToExpand.zipWithIndex
          (rightPattern, rightIndex) <- patternsToExpand.zipWithIndex
          if leftIndex != rightIndex
          candidate <- combinePatterns(leftPattern, rightPattern)
        } yield candidate
      }

      override def next(): List[SequentialPattern] = {
        k = k + 1
        val candidates = generateCandidates(k, lastPatterns)
        val patterns = candidates.filter(_.support(data) >= minSupport)
        //println(s"Found ${patterns.size} from ${candidates.size} for k=$k")
        val tmp = lastPatterns
        lastPatterns = patterns
        tmp.toList
      }
    }
    val patterns = generator.takeWhile(_.nonEmpty).flatten
    patterns
  }
}

case class RunResults(patternCount: Int, prepareTime: FiniteDuration, runTime: FiniteDuration)

trait GspCommons {
  val minSupport = System.getenv("MIN_SUPPORT").toDouble
  val dataFile = System.getenv("DATA_FILE")
  val logger = LoggerFactory.getLogger(getClass)

  import model._

  def lineToDataSequence(line: String): DataSequence = {
    val parts = line.trim.split(' ')
    val partIterator = parts.takeWhile(_ != "-2").iterator
    val transactions = partIterator.map { tIdString =>
      require(tIdString.startsWith("<"))
      require(tIdString.endsWith(">"))
      val tId = tIdString.substring(1, tIdString.length - 1)
      val items = partIterator.takeWhile(_ != "-1").map(itemIdString => Item(itemIdString.toInt)).toSet
      Transaction(items, tId.toLong)
    }
    DataSequence(transactions.toList)
  }

  def loadData: Data

  implicit def stopwatchToDuration(stopwatch: Stopwatch): FiniteDuration = FiniteDuration(stopwatch.elapsed(TimeUnit.MILLISECONDS), TimeUnit.MILLISECONDS)

  def execute(): RunResults = {
    val prepareStopwatch = new Stopwatch().start()
    val loadedData = loadData
    prepareStopwatch.stop()
    val runStopwatch = new Stopwatch().start()
    val patterns = Gsp.mineGsp(loadData, Support(minSupport)).zipWithIndex
    val count = patterns.map { case (pattern, index) =>
      logger.warn(s"Pattern $index found: $pattern")
      index
    }.size
    val results = RunResults(count, prepareStopwatch, runStopwatch)
    println(results)
    results
  }
}

object GspPureApp extends App with GspCommons {

  import sparkfixtures._

  def loadData = {
    val sequences = scala.io.Source.fromFile(dataFile).getLines()
      .map(lineToDataSequence)
      .cache()
    model.DataCollection(sequences)
  }

  execute()
}

object GspSparkApp extends App with GspCommons {

  val conf = new SparkConf().setAppName(getClass.getSimpleName).set("spark.eventLog.enabled", "true")
  val sc = new SparkContext(conf)
  def loadData = {
    val sequences = sc.textFile(dataFile)
      .map(lineToDataSequence)
      .cache()
    model.DataRdd(sequences)
  }

  execute()
}
