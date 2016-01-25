import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf


object Spade {
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


  case class Node(sequence: List[Atom], idList: IdList) extends Serializable {
    var support = idList.support

    override def toString(): String =
      this.sequence.reverse
        .map(a => {
          a match {
            case SequenceAtom(i) => "-> " + i
            case EventAtom(i)    => " " + i
          }
        })
        .reduce(_+_) + " [%s]".format(this.support)

    def prefix: List[Atom] = sequence.tail

    def join(right: Node): List[Node] = {
      val Node(al :: prefix, idsLeft) = this
      val Node(ar :: prefixRight, idsRight) = right
      require(prefix == prefixRight,
              "expected " + this + " and " + right + " to have identical prefixes")

      def tj() = idsLeft.temporalJoin(idsRight)
      def ej() = idsLeft.eventJoin(idsRight)
      def makeNode(a1: Atom, a2: Atom, idList: IdList) =
        Node(a2 :: a1 :: prefix, idList)

      (al, ar) match {
        case (EventAtom(il), EventAtom(ir)) =>
          if (il >= ir) List()
          else          List(makeNode(al, ar, ej))

        case (EventAtom(il), SequenceAtom(ir)) =>
          List(makeNode(al, ar, tj))

        case (SequenceAtom(il), EventAtom(ir)) =>
          List()

        case (SequenceAtom(il), SequenceAtom(ir)) =>
          if (il < ir)       List(makeNode(al, ar, tj),
                                  makeNode(al, EventAtom(ir), ej))
          else               List(makeNode(al, ar, tj))
      }
    }
  }

  def makeAtomNode(item: String, ids: List[(Long, Long)]) = {
    Node(List(SequenceAtom(item)), IdList(ids))
  }

  def enumerateFrequentSeq(minSup: Long, atomNodes: Iterable[Node]): Iterable[Node] = {
    def generateClass(leftAtom: Node): Iterable[Node] =
      atomNodes.flatMap(leftAtom.join(_)).filter(_.support >= minSup)

    val prefixClasses = atomNodes.map(generateClass)

    prefixClasses.flatMap(enumerateFrequentSeq(minSup, _)) ++ atomNodes
  }

}


object SpadeSparkApp {
  def readLine(line: String) : (Long, Long, Iterable[String]) = {
    // wczytaj linię o składni:
    // <sid> <eid> <item1> ... <itemN>
    val ls = line.split(" ")
    val sid = ls(0).toLong
    val eid = ls(1).toLong
    val items = ls.slice(2, ls.length)
    (sid, eid, items)
  }

  def loadHorizontalDb(ctx: SparkContext, path: String) =
    ctx.textFile(path).map(readLine)

  def main(args: Array[String]) {
    val horizontalDbPath = args(0)
    val minSup = args(1).toLong
    val conf = new SparkConf().setAppName(s"SPADE ${horizontalDbPath} ${minSup}")
    val sc = new SparkContext(conf)

    val frequentItems = loadHorizontalDb(sc, horizontalDbPath)
      .flatMap(event => {
        val (sid, eid, items) = event
        items.map(item => (item, (sid, eid)))
      })
      .groupByKey()
      .map(item => Spade.makeAtomNode(item._1, item._2.toList))
      .filter(_.support >= minSup)
      .cache()

    val frequent2Sequences = frequentItems.cartesian(frequentItems)
                                      // Dla każdej pary przedmiotów częstych
      .flatMap(p => p._1.join(p._2))  // złącz ją aby otrzymać sekwencje długości 2,
      .filter(_.support >= minSup)    // Wybierz sekwencje częste.
      .cache()

    val classFrequentSequences = frequent2Sequences
      .map(n => (n.prefix, n))  // Wybierz klasy równoważności sekwencji
                                // o wspólnych prefiksach długości 1,
      .groupByKey()             // Połącz je we wspólne wiersze RDD,
      .flatMap(group => Spade.enumerateFrequentSeq(minSup, group._2))
                                // Znajdź wszystkie sekwencje częste generowane
                                // w klasach równoważności.

    val allFrequentSequences = frequentItems.union(classFrequentSequences)

    allFrequentSequences.foreach(println)
  }
}


object SpadePureApp {
  def readLine(line: String) : (Long, Long, Iterable[String]) = {
    // wczytaj linię o składni:
    // <sid> <eid> <item1> ... <itemN>
    val ls = line.split(" ")
    val sid = ls(0).toLong
    val eid = ls(1).toLong
    val items = ls.slice(2, ls.length)
    (sid, eid, items)
  }

  def main(args: Array[String]) {
    val horizontalDbPath = args(0)
    val minSup = args(1).toLong

    val frequentItems = scala.io.Source.fromFile(horizontalDbPath).getLines()
      .map(readLine)
      .flatMap(event => {
        val (sid, eid, items) = event
        items.map(item => (item, (sid, eid)))
      })
      .toList
      .groupBy(_._1)
      .map(item => Spade.makeAtomNode(item._1, item._2.map({_._2})))
      .filter(_.support >= minSup)

    val allFrequentSequences = Spade.enumerateFrequentSeq(minSup, frequentItems)
    allFrequentSequences.foreach(println)
  }
}
