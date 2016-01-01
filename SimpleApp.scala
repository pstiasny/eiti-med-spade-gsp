import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf


abstract class Atom
case class EventAtom(item: String) extends Atom
case class SequenceAtom(item: String) extends Atom


class IdList(events: Array[(Long, Long)]) extends Serializable {
  def temporalMatch(right: IdList): IdList = this
  def eventMatch(right: IdList): IdList = this
  def support: Long = {
    val sids = this.events.map(_._1)
    if (sids.length == 0) return 0
    sids.zip(sids.tail)
      .map(p => if (p._1 == p._2) 0 else 1)
      .fold(0)(_+_) + 1
  }
}


case class Node(sequence: Array[Atom], idList: IdList) extends Serializable {
  var support = idList.support

  override def toString() = {
    var s = ""
    this.sequence.foreach(a => {
      a match {
        case SequenceAtom(i) => s += "-> " + i
        case EventAtom(i)    => s += " " + i
      }
    })

    s += " [%s]".format(idList.support)
    s
  }
}


object SimpleApp {
  def makeAtomNode(item: String, ids: Array[(Long, Long)]) = {
    new Node(Array(new SequenceAtom(item)), new IdList(ids))
  }

  def printSequence(as: Array[Atom]) {
    as.foreach(a => {
      a match {
        case SequenceAtom(i) => print("-> " + i)
        case EventAtom(i)    => print(" " + i)
      }
    })
    println("");
  }


  def main(args: Array[String]) {
    val horizontalDbPath = "/home/pawel/dev/bolidupa/med/projekt/db.horizontal"
    val minSup = 2
    val conf = new SparkConf().setAppName("Simple Application")
    val sc = new SparkContext(conf)

    val frequentItems = sc.textFile(horizontalDbPath)
      .map(line => {
        val ls = line.split(" ")
        val sid = ls(0).toLong
        val eid = ls(1).toLong
        val items = ls.slice(2, ls.length)
        (sid, eid, items)
      })
      .flatMap(event => {
        val (sid, eid, items) = event
        items.map(item => (item, (sid, eid)))
      })
      .groupByKey()
      .map(item => makeAtomNode(item._1, item._2.toArray))
      .filter(_.support >= minSup)
      .cache()

    frequentItems.foreach(node => println(node))
  }
}
