package streams
import common._

trait GameDef {
  case class Pos(x: Int, y: Int) {
    def dx(d: Int) = copy(x = x + d)
    def dy(d: Int) = copy(y = y + d)
  }
  val startPos: Pos
  val goal: Pos

  type Terrain = Pos => Boolean
  val terrain: Terrain

  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  case class Block(b1: Pos, b2: Pos) {
    require(b1.x <= b2.x && b1.y <= b2.y, "Invalid block position: b1=" + b1 + ", b2=" + b2)
    def dx(d1: Int, d2: Int) = Block(b1.dx(d1), b2.dx(d2))
    def dy(d1: Int, d2: Int) = Block(b1.dy(d1), b2.dy(d2))
    def left = if (isStanding)         dy(-2, -1)
               else if (b1.x == b2.x)  dy(-1, -2)
               else                    dy(-1, -1)
    def right = if (isStanding)        dy(1, 2)
                else if (b1.x == b2.x) dy(2, 1)
                else                   dy(1, 1)
    def up = if (isStanding)           dx(-2, -1)
             else if (b1.x == b2.x)    dx(-1, -1)
             else                      dx(-1, -2)
    def down = if (isStanding)         dx(1, 2)
               else if (b1.x == b2.x)  dx(1, 1)
               else                    dx(2, 1)
    // ex. 3a
    def neighbors: List[(Block, Move)] = List((left, Left), (right, Right), (up, Up), (down, Down))
    // ex. 3b
    def legalNeighbors: List[(Block, Move)] = neighbors.filter(_._1.isLegal)
    // ex. 2a
    def isStanding: Boolean = b1 == b2
    // ex. 2b
    def isLegal: Boolean = terrain(b1) && terrain(b2)
  }
  // ex. 2c
  def startBlock: Block = Block(startPos, startPos)
}

trait StringParserTerrain extends GameDef {
  val level: String
  // ex. 1a
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = (p: Pos) => {
    if (p.x < 0 || p.y < 0 || p.x >= levelVector.size || p.y >= levelVector(0).size)
      false
    else
      levelVector(p.x)(p.y) != '-'
  }
  // ex. 1b
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val x = levelVector.indexWhere((v: Vector[Char]) => v.contains(c))
    val y = levelVector(x).indexOf(c) // searches the row x twice...
    Pos(x,y)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)
}

trait Solver extends GameDef {
  // ex. 4a
  def done(b: Block): Boolean = b.isStanding && b.b1 == goal

  // ex. 4b
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    // this implementation defeats the purpose of lazy streams...
    // but it should be OK, as legalNeighbors has at most 4 items
    (for (neighbor <- b.legalNeighbors)
     yield (neighbor._1, neighbor._2 :: history)).toStream
  }

  // ex. 4c
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    neighbors.filter((x: (Block, List[Move])) => !explored.contains(x._1))
  }

  // ex. 4d
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if (initial.isEmpty)
      Stream()
    else {
      val next =
        initial.flatMap((x) => newNeighborsOnly(neighborsWithHistory(x._1, x._2), explored))
      initial #::: from(next, explored ++ initial.map(_._1))
    }
  }

  // ex. 4e/1
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock, List())), Set())
  // ex. 4e/2
  lazy val pathsToGoal: Stream[(Block, List[Move])] =
    pathsFromStart.filter((x: (Block, List[Move])) => done(x._1))
  // ex. 4e/3
  lazy val solution: List[Move] = if (pathsToGoal.isEmpty) Nil else pathsToGoal.head._2.reverse
}
