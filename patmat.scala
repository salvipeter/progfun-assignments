package patmat
import common._

object Huffman {
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // ex. I/1
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  // ex. I/2
  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  // ex. II/1
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case List() => List()
    case x :: xs => {
      def isX(y: Char) = x == y
      (x, chars.count(isX)) :: times(xs.filterNot(isX))
      }
  }

  // ex. II/2
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def comp(a: (Char, Int), b: (Char, Int)) = a._2 < b._2
    freqs.sortWith(comp).map((x: (Char, Int)) => new Leaf(x._1, x._2))
  }

  // ex. II/3
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case _ :: List() => true
    case _ => false
  }

  // ex. II/4
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case x :: y :: xs => {
      val new_elem = makeCodeTree(x, y)
      def rec(list: List[CodeTree]): List[CodeTree] = list match {
        case List() => List(new_elem)
        case t :: ts => if (weight(t) < weight(new_elem)) t :: rec(ts) else new_elem :: t :: ts
      }
      rec(xs)
    }
    case _ => trees
  }

  // ex. II/5
  def until(s: List[CodeTree] => Boolean,
            c: List[CodeTree] => List[CodeTree])(t: List[CodeTree]): List[CodeTree] =
    if (s(t)) t else until(s, c)(c(t))

  // ex. II/6
  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int

  // ex. III/1
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def rec(t: CodeTree, b: List[Bit]): List[Char] = {
      if (b.isEmpty) chars(t)
      else t match {
        case Leaf(c, _) => c :: rec(tree, b)
        case Fork(l, r, _, _) => if (b.head == 0) rec(l, b.tail) else rec(r, b.tail)
      }
    }
    rec(tree, bits)
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  // ex. III/2
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // ex. IV/1
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def rec(t: CodeTree, c: Char): List[Bit] = t match {
      case Leaf(_, _) => List()
      case Fork(l, r, _, _) => if (chars(l).exists((x: Char) => x == c)) 0 :: rec(l, c)
                               else 1 :: rec(r, c)
    }
    text match {
      case List() => List()
      case x :: xs => rec(tree, x) ::: encode(tree)(xs)
    }
  }

  type CodeTable = List[(Char, List[Bit])]

  // ex. IV/2
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    def rec(t: CodeTable): List[Bit] = t match {
      case List() => ???
      case x :: xs => if (x._1 == char) x._2 else rec(xs)
    }
    rec(table)
  }

  // ex. IV/3
  def convert(tree: CodeTree): CodeTable = {
    tree match {
      case Leaf(c, _) => List((c, List()))
      case Fork(l, r, _, _) => mergeCodeTables(convert(l), convert(r))
    }
  }

  // ex. IV/4
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    def prefix(p: Bit)(x: (Char, List[Bit])) = (x._1, p :: x._2)
    a.map(prefix(0)) ::: b.map(prefix(1))
  }

  // ex. IV/5
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =
    text.map(codeBits(convert(tree))).reduce(_++_)
}
