package com.intel.cg.hpfd.madisonbay.wm.example;

/** Implements A-star search of a text format maze
  *
  *  This is Mika's programming assignment for perspective technical hires.
 */
object AstarSearch  {

  /**
    * Representation of position within the maze
    * @param x location along the horizontal axis
    * @param y location along the vertical axis (0 is the 'top')
    */
  case class Position(x: Int, y: Int) {
    def neighbors: Iterator[Position] = {
      val result = List(Position(x - 1, y), Position(x + 1, y), Position(x, y - 1), Position(x, y + 1))
      result.toIterator
    }
  }


  /**
    * Load a maze into memory, from file
    * @param file file matching Mika's expected syntax
    * @return representation of the maze
    */
  def readProblem(file: String) : (Position, Position, Int, Int, Set[Position]) = {
    val it = Source.fromFile(file).getLines
    val xdim = it.next.trim.toInt
    val ydim = it.next.trim.toInt
    var blockages = collection.mutable.HashSet[Position]()
    var goal: Option[Position] = None
    var source: Option[Position] = None
    for (y <- 0 until ydim) {
      val line = it.next
      val pointsOnLine = line.split(" ")
      assert(pointsOnLine.size == xdim, "Expected xdim = " + "but found line with " + pointsOnLine.size + " points")
      var x = 0
      for (p <- pointsOnLine) {
        val pos = Position(x,y)
        x += 1
        p.toLowerCase match {
          case "x" => blockages += pos
          case "g" => goal = Option(pos)
          case "s" => source = Option(pos)
          case "_" => None
          case _ => assert(false, "problematic string: " + p)
        }
      }
    }
    assert(goal.isDefined)
    assert(source.isDefined)
    val blockagesResult = blockages.toIterator.toSet // return as an immutable set
    (source.get, goal.get, xdim, ydim, blockagesResult)
  }

  /**
    * The meat of the A star implementation
    * @param source
    * @param goal
    * @param blockages
    * @param xdim
    * @param ydim
    * @return a list of points showing the solution, or 'none' if there is no solution
    */
  def astar(source: Position, goal: Position, blockages: Set[Position], xdim: Int, ydim: Int) : Option[List[Position]] = {
    def futureCost(pos: Position): Int = {
      scala.math.abs(pos.x - goal.x) + (pos.y - goal.y)
    }
    val predecessorPosition = scala.collection.mutable.HashMap[Position,Position]()
    val g = scala.collection.mutable.HashMap[Position,Int]()
    val closed = scala.collection.mutable.HashSet[Position]()

    def offBoard(p: Position) : Boolean = {
      p.x < 0 || p.y < 0 || p.x >= xdim || p.y >= ydim
    }

    case class NodeCost(p: Position, fCost:Int) extends Ordered[NodeCost] {
      override def compare(that: NodeCost): Int = {
        if (this.fCost == that.fCost)
          this.p.hashCode() compare that.p.hashCode()
        else
          that.fCost compare this.fCost
      }
    }

    val pq  = mutable.PriorityQueue[NodeCost]()
    g.put(source, 0)
    pq.enqueue(NodeCost(source, futureCost(source)))

    while(pq.nonEmpty) {
      val nextToExplore = pq.dequeue
      val node = nextToExplore.p
      closed.add(node)
      if (node == goal) {
        println("Reached goal!")
        // finish up
        pq.dequeueAll
        object AnswerIterator extends Iterator[Position] {
          var p = Option(node)
          def hasNext: Boolean = {
            p.isDefined
          }
          def next: Position =  {
            val toReturn = p.get
            p = predecessorPosition.get(toReturn)
            toReturn
          }
        }
        return Option(AnswerIterator.toList)
      } else {
        val gCost = g(node)
        for (n <- node.neighbors.filterNot(blockages.contains).filterNot(closed.contains).filterNot(offBoard)) {
          // fcost = gcost + hcost
          val thisgcost = gCost + 1
          val hcost = futureCost(n)
          val oldG = g.get(n)
          def updateNode(): Unit = { g.put(n, thisgcost) ; predecessorPosition.put(n,node); pq.enqueue(NodeCost(n, thisgcost + hcost)) }
          oldG match {
            case None => updateNode()
            case Some(cost) if cost > thisgcost => updateNode()
            case _ => None
          }
        }
        while (pq.nonEmpty && closed.contains(pq.head.p))
          pq.dequeue
      }
    }
    None
  }

  /**
    * print a text representation of the maze, and the solution, if it exists
    * @param xdim
    * @param ydim
    * @param source
    * @param goal
    * @param blockages
    * @param result
    */
  def printMaze(xdim: Int, ydim: Int,
                source: Position, goal: Position, blockages: Set[Position], result: Option[List[Position]]): Unit = {
    // build up the result into a set, so we can quickly lookup within in
    val pathPosition = result.getOrElse(List[Position]()).toSet
    for (y <- 0 until ydim) {
      for (x <- 0 until xdim) {
        val pos = Position(x,y)
        val character = {
          pos match {
            case `source` => "s"
            case `goal` => "g"
            case _ => if (blockages.contains(pos)) "x"
            else if (pathPosition.contains(pos)) "o"
            else "-"
          }
        }
        print(character + " ")
      }
      println
    }
  }

  def main(args: Array[String]): Unit = {
    val (source, goal, xdim, ydim, blockages) = readProblem(args(0))
    val resultPath = astar(source, goal, blockages, xdim, ydim)
    printMaze(xdim, ydim, source, goal, blockages, resultPath)
  }
}
