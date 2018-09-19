package com.intel.cg.hpfd.madisonbay.wm.example

import org.scalatest._

class AstarSpec extends FunSpec  {
  def computeExampleFilePath(f : String) : String = {
    val sep = java.io.File.separator
    "src" + sep + "test" + sep + "data" + sep + f
  }
/*
  describe ("5.txt") {
    it("should have a no path found ") {
      val (source, goal, xdim, ydim, blockages) = AstarSearch.readProblem(computeExampleFilePath("5.txt"))
      val resultPath = AstarSearch.astar(source, goal, blockages, xdim, ydim)
      assert(resultPath.isEmpty)
    }
  }
  describe ("11.txt") {
    val dim = 20
    val blockageCount = 46
    val optimalLength = 44
    it("should have xdim and ydim == " + dim) {
      val (_, _, xdim, ydim, _) = AstarSearch.readProblem(computeExampleFilePath("11.txt"))
      assert(xdim == dim)
      assert(ydim == dim)
    }
    it("should have " + blockageCount + " blockages") {
      val (_, _, _, _, blockages) = AstarSearch.readProblem(computeExampleFilePath("11.txt"))
      assert(blockages.size == blockageCount)
    }
    it("should have a path length of " + optimalLength) {
      val (source, goal, xdim, ydim, blockages) = AstarSearch.readProblem(computeExampleFilePath("11.txt"))
      val resultPath = AstarSearch.astar(source, goal, blockages, xdim, ydim)
      assert(resultPath.isDefined)
      assert(resultPath.get.size == optimalLength)
    }
  }*/
}
