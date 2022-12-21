package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day12 extends AdventDay(12) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  val grid: Array[Array[Char]] = lines.map(_.toCharArray).toArray
  
  val height: Int = grid.size
  val width: Int = grid.head.size
  
  val yMax: Int = height - 1
  val xMax: Int = width - 1
  
  sealed abstract class Direction(complement: Direction)
  case object Up extends Direction(Down)
  case object Down extends Direction(Up)
  case object Left extends Direction(Right)
  case object Right extends Direction(Left)
  
  case class Point(x: Int, y: Int) {
    val up: Option[Point] = pointFrom(this, Up)
    val down: Option[Point] = pointFrom(this, Down)
    val left: Option[Point] = pointFrom(this, Left)
    val right: Option[Point] = pointFrom(this, Right)
  }
  
  def pointFrom(point: Point, direction: Direction): Option[Point] = (point, direction) match {
    case (Point(_, 0), Up) => None
    case (Point(_, `yMax`), Down) => None
    case (Point(0, _), Left) => None
    case (Point(`xMax`, _), Right) => None
    case (Point(x, y), Up) => Some(Point(x, y - 1))
    case (Point(x, y), Down) => Some(Point(x, y + 1))
    case (Point(x, y), Left) => Some(Point(x - 1, y))
    case (Point(x, y), Right) => Some(Point(x + 1, y))
  }
  
  class Path {
    private var _score: Int = 0
    private var _visited: Set[Point] = Set.empty
    private var _segments: List[Point] = Nil
    
    def hasVisited(point: Point): Boolean = _visited.contains(point)
  }
  
  /*
  case class Border(point: Point, direction: Direction, companion: Option[Border])
  case class Plot(point: Point, level: Int) {
    val top: Option[Border] = {
      Border(point, Up, pointFrom(point).map(p => Border(point, Down, this)))
    }
    val bottom: Option[Border] =
    val left: Option[Border] =
    val right: Option[Border] =
  }
  
  def companionFor(border: Border): Option[Border] = border match {
    case Border(Point(), Up) => Border()
    case Border(Point(), Down) => Border()
    case Border(Point(), Left) => Border()
    case Border(Point(), Right) => Border()
  }
  
  val borderMap: Map[Border, Plot] = grid.zipWithINdex.flatMap({
  
  })
  
  object Plot {
    def at(x: Int, y: Int, value: Char): Plot = {
    
    }
  }
  */
  
  object p1 {
    def answer: String = ""
  }
  
  object p2 {
    def answer: String = ""
  }
}