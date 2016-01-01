import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf


abstract class Atom
case class EventAtom(item: String) extends Atom
case class SequenceAtom(item: String) extends Atom


case class IdList(events: List[(Long, Long)]) extends Serializable {
  type Id = (Long, Long)

  def temporalJoin(right: IdList): IdList = {
    def tm(l: List[Id], r: List[Id]): List[Id] = {
      if (l.isEmpty || r.isEmpty)
        return List()

      val (lsid, leid)::lt = l
      val (rsid, reid)::rt = r

      if (lsid < rsid)       tm(lt, r)
      else if (lsid > rsid)  tm(l, rt)
      else if (leid < reid)  (rsid, reid) :: tm(l, rt)
      else                   tm(l, rt)
    }

    IdList(tm(this.events, right.events))
  }

  def eventJoin(right: IdList): IdList = {
    def em(l: List[Id], r: List[Id]): List[Id] = {
      if (l.isEmpty || r.isEmpty)
        return List()

      val (lsid, leid)::lt = l
      val (rsid, reid)::rt = r

      if (lsid < rsid)       em(lt, r)
      else if (lsid > rsid)  em(l, rt)
      else if (leid < reid)  em(lt, r)
      else if (leid == reid) (rsid, reid) :: em(l, rt)
      else                   em(l, rt)
    }

    IdList(em(this.events, right.events))
  }

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
  def makeAtomNode(item: String, ids: List[(Long, Long)]) = {
    Node(Array(SequenceAtom(item)), IdList(ids))
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
      .map(item => makeAtomNode(item._1, item._2.toList))
      .filter(_.support >= minSup)
      .cache()

    frequentItems.foreach(node => println(node))
  }
}
