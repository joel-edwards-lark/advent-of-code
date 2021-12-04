package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}
import com.buzuli.util.Time

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object day4 extends AdventDay(4) {
  override def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    val r1 = puzzle1
    val r2 = puzzle2
    for {
      p1 <- r1
      p2 <- r2
    } yield {
      success(s"""Puzzle1 => ${p1} | Puzzle2 => ${p2}""")
    }
  }

  def puzzle1(implicit ec: ExecutionContext): Future[Int] = Future {
    val (duration, value) = Time.timing {
      val boards = data.boards
      var result: Option[Int] = None

      data.selections.takeWhile { value =>
        boards.takeWhile { board =>
          result = board.mark(value)
          result.isEmpty
        }
        result.isEmpty
      }

      result.getOrElse(0)
    }

    logger.info(s"${name} Puzzle 1 => ${value} (${Time.prettyDuration(duration)})")

    value
  }

  def puzzle2(implicit ec: ExecutionContext): Future[Int] = Future {
    val (duration, value) = Time.timing {
      var result: Option[Int] = None
      var boards = data.boards

      data.selections.takeWhile { value =>
        boards = boards.filter { board =>
          board.mark(value) match {
            case None => true
            case Some(score) => {
              result = Some(score)
              false
            }
          }
        }
        boards.nonEmpty
      }

      result.getOrElse(0)
    }

    logger.info(s"${name} Puzzle 2 => ${value} (${Time.prettyDuration(duration)})")

    value
  }

  object data {
    val (_selections: String, _boards: List[String]) = {
      {
        raw
          .split("\n\n")
          .view
          .map(_.trim)
          .filter(_.nonEmpty)
          .toList
      } match {
        case s :: b => (s, b)
      }
    }

    val selections: List[Int] = {
      _selections
        .split(",")
        .map(_.toInt)
        .toList
    }

    def boards: List[Board] = _boards.map { Board }

    case class Board(
      boardText: String,
    ) {
      val matrix: List[List[Int]] = {
        boardText
          .split("\n")
          .view
          .map(_.trim)
          .filter(_.nonEmpty)
          .map { line =>
            line
              .split(" ")
              .view
              .map(_.trim)
              .filter(_.nonEmpty)
              .map(_.toInt)
              .toList
          }.toList
      }

      val width: Int = matrix.length
      val height: Int = matrix.head.length

      // Map the value back to the coordinates
      val valueToCoordinateMap: mutable.Map[Int, (Int, Int)] = {
        mutable.Map.from(
          matrix.zipWithIndex.flatMap({ case (subList, y) =>
            subList.zipWithIndex.map({ case (value, x) =>
              (value, (x, y))
            })
          })
        )
      }

      var colCounts: mutable.Map[Int, Int] = mutable.Map()
      var rowCounts: mutable.Map[Int, Int] = mutable.Map()

      /**
       * Mark the position on the board, indicating a bingo by returning the score.
       *
       * @param value the value to mark
       *
       * @return Some(score) if marking this value resulted in a bingo
       */
      def mark(value: Int): Option[Int] = {
        valueToCoordinateMap.remove(value) flatMap { case (x, y) =>
          val xBingo = colCounts
            .updateWith(x)(_.orElse(Some(0)).map(_+1))
            .exists(_ >= width)

          val yBingo = rowCounts
            .updateWith(y)(_.orElse(Some(0)).map(_+1))
            .exists(_ >= height)

          val bingo = xBingo || yBingo

          Option.when(bingo)(valueToCoordinateMap.keys.sum * value)
        }
      }
    }

    lazy val raw: String = {
      """
      |62,55,98,93,48,28,82,78,19,96,31,42,76,25,34,4,18,80,66,6,14,17,57,54,90,27,40,47,9,36,97,56,87,61,91,1,64,71,99,38,70,5,94,85,49,59,69,26,21,60,0,79,2,95,11,84,20,24,8,51,46,44,88,22,16,53,7,32,89,67,15,86,41,92,10,77,68,63,43,75,33,30,81,37,83,3,39,65,12,45,23,73,72,29,52,58,35,50,13,74
      |
      |10 83 98 12 33
      |38 68  2 99 85
      |16 89 54 50 97
      |31  8 17 11 76
      | 0 55 66 32 87
      |
      |77 60 61 59 16
      |17 46 97  9 73
      |42 82 25 32 29
      |48 94 52 55 50
      |95 14 67 79 34
      |
      | 6 31  5 74 67
      |76 89 15 78 47
      |54 49 62 32 38
      |35 43 61 22 58
      |75 97 33  8 16
      |
      |37 58 22 47 30
      | 4 86 77 42 54
      |73 94 87 34 55
      |72 53 14 64 88
      |23 21 36 52 66
      |
      |59 16 18 85 93
      |96 56 50 53 94
      |68 95 77  0 27
      |47 30 88 46 65
      |75 82 41 31 23
      |
      |88 96 94 75  3
      |31 26 74 90 36
      |59 22 41 84 54
      | 6 68 37 20 93
      |10 66 46  9 79
      |
      |33 78 12 62 73
      |18 37  7 44 66
      |69 15 54 53 82
      |98 20 30 58 81
      |56 95 36 91 99
      |
      |47 89 86 58 43
      |49 72 84 94 51
      |69 73 15 50  0
      |46 81 76 31 61
      |96 91 56  2 34
      |
      |22 91 70 68 99
      |25 28  3 42  1
      |21 19 79 54  4
      |97 88 49  8 78
      |44 98 84 83  6
      |
      |21  0 62  2  9
      |49 81 50 66 10
      |24 72 45 96  6
      |51 82 17 58 91
      |18 56 39 11 25
      |
      |22 25 42 98 93
      | 1 26  0 67 65
      |31 11 37  7 96
      |28 17 40 79 12
      |95  5  2 66 10
      |
      |63  8 97 64 82
      |43 12 83  1 11
      |69 84 74  7 59
      |25 48 38 89 62
      |22 93 39 71 76
      |
      |92 94 20 49 21
      |34 41 61 98 28
      |93 62 74 12 31
      |63 77 87 36 55
      |23  7  4 69 53
      |
      |61 27 76 56 12
      |60  7 36 33 97
      | 4 16 89 44 40
      |51 43 75 79 28
      |14 69 35 90  8
      |
      |12 34 94 77 87
      |24 61 19 23 41
      |75 46  9 21 64
      |88 15 40 89 86
      |32 47 93  3 58
      |
      |74 16 44 21 30
      | 1 11 71 97 42
      |88 59 52 28 75
      |58 26 23 76  8
      |33 65 80 95 14
      |
      |54 29  7 80 33
      |20 18 82 26 93
      |72  4 45 89 98
      |99 16  8 22 34
      |86 61 51 43 64
      |
      |96 20 88 78 73
      |65 84 93 79 48
      |25 99 13 60 41
      |37 24 82  8 89
      |44 10  4 58 57
      |
      |28 70 42 66 15
      | 3 35 52 49  4
      |77 23 16 30 24
      |67 75  8 29 47
      |39 32 80 22 55
      |
      |70 61  5 77  9
      | 3 23 42 86 31
      |99 22 41 14 17
      |93 63 25 10 30
      |28 52 81 89 40
      |
      |78 18 42 48 14
      | 4 95 87 64 32
      |13 10 72 90 46
      |68 16 57 80 77
      |50 69  5 63 96
      |
      |89 15 13 68 84
      |37 79 56 97 34
      |60 48 91 87 96
      |32 25 78 55 11
      | 1 67 57 93 92
      |
      |89 94 12 46 21
      |61 67 26 40 76
      |86 78  6 41 56
      |35 64 28 73 98
      |30 17 88 70 71
      |
      |37 57 36  6 32
      |89 26 27 22 29
      |80 49 88  0 46
      |70 18 50 14 19
      |34 84 79 90 98
      |
      |41 23 10  4 88
      |26 55 17 71 15
      |68 49  0 14 97
      |27 61 31 74 99
      |89 33 64 32  5
      |
      |63 44 98 56 47
      |72  2 28 89 77
      |36 24 26 14 21
      | 7 58 32 31 86
      |33  0 57 54  4
      |
      | 2 89 46 59  6
      |62 67 84 95 98
      | 8 12 75 70 88
      |45 93 38 61 47
      |37 55 76 82 92
      |
      | 2 50 19 35 34
      |94  0 48 75 16
      |18 92 46 38 32
      |65 78 22 85 77
      |69 73 88 30 60
      |
      |98 21 79 41 39
      |64  1 91  7 44
      |45 32 72 22 38
      |78 28 97 69 33
      |55 12 53  9 61
      |
      |94 39 67 82 18
      |11 86 43 92  0
      |44  8 66  3 91
      |62 56 38 32 89
      |27  2 76 90 31
      |
      |21 79 89 70 85
      |73 76 92 15 33
      |36 63 44 99 19
      |35 75 88 65  3
      |48 54 97 27  2
      |
      |35  8 51 77 29
      | 1 11 38 67 99
      | 2 18 94 32 24
      |54 82 21 98  7
      |20  0 48 83 74
      |
      |77 82 68 18 58
      | 9 78 85 59 55
      |15 73 56 46 10
      |80 38 26  8 96
      |41 84 35 86 12
      |
      |36 89 27 38 22
      |53 46  5 84 90
      |23  7 63 29 17
      |92 41 97  0 43
      |74 33 26 98 19
      |
      |69 40 35 84  3
      |56 49 55  2 28
      |85 14 50 12 27
      |65 73  6 42 23
      |64 68 48 62 22
      |
      |57  8 21 98 66
      |39 92 16 95 87
      |49  1 51 68 48
      |46 84 17 35 80
      |20 47  3 75 34
      |
      |23 15 77  3 91
      |33 58 69 66 14
      |88 47 18 16 99
      |62 89 86  7 67
      |90 57 35 45 29
      |
      |89 12 29 39 78
      |26 52 10 47 97
      |68 90 65 56 33
      |63  8 13 27 42
      |30 66 91 16 51
      |
      |95 91 88 40 97
      |63 54 68 26 52
      |56 76 78 83 62
      |13 65 90 49 94
      |44 74 79 48 81
      |
      |24 27 11 74  0
      |38 56 53 25 60
      |50 51 49 10 72
      |76 34 52 81  9
      |80 99 82  1 67
      |
      |90 88 71 53 26
      |70 19 57 61 89
      |64 30  0  9 56
      | 4 21 62 38 82
      |51 40 55 81 20
      |
      |66 14 90 76 93
      |21 57 27 55 32
      |22 43 67 29 81
      |49 53 39 96 79
      |12 48 88 63 33
      |
      |15  7 99 55 84
      |53 80 47 75 36
      | 1 22 39 91 82
      |13 76 40 27 81
      |57 93  8 48 28
      |
      |82 58  5 84 25
      |61 19 83 22 44
      |85  3 14 10 97
      |35 26 79 20 73
      |99 21 51 47 81
      |
      |14 32 63 18 70
      |50 91 67  1 19
      | 7 31 54 11  8
      |51 78 35 72 77
      |47 73 22  5 76
      |
      | 2 22 11 31 13
      |66 21 83 94 87
      |69  5 59 14 53
      |95 41 90 43 92
      |42 77 10 88  8
      |
      |19 88 91 43 17
      |62 83 68 94 28
      |73 36 58 21 66
      |55 24 90 12 77
      |45  6 49 27 63
      |
      | 6 73 93 67 18
      |29 33 94 24 34
      |57 96 27 37 60
      |92 88 81 12 16
      |49 98 30 10 72
      |
      |17 40 36 96 68
      |91 38 88  9  6
      |16 35 63 25 37
      |81 43 78 64 52
      |46 44 69 67 13
      |
      |73 36 70  5 57
      |66 55 27 54 44
      |20  4 68 58 26
      |96 37 76 80 47
      |61  2 92 71 64
      |
      |12 51 54 34 68
      |69 99 64 44 98
      |79 67 90 46 65
      |31 56 45 43 30
      |29 18 94 19 59
      |
      |70 26 91 97  0
      |46 56 93 80 52
      |47 25 12 31 77
      |95 72 36 74  2
      |38 48 68 54 73
      |
      |79 89 77 24 21
      |30 14 46 11 38
      | 3 70 84 67 48
      |45 20 40 63 35
      |86 74  2 76 43
      |
      |97 20  2 82 84
      | 8 92 71 88 33
      |64 26 99 93 66
      |30 40 28 38 73
      |62 43  5 81 22
      |
      | 9 47 50  8 62
      |42  1 80 21 84
      |66 19 32  2 30
      |76 97 85 65 45
      |70 26 73 72 93
      |
      |80 99 91 96 25
      |22 76 81 62 51
      |10 64 53 54 70
      |55  8 49 60  1
      |40 67 14 89 16
      |
      |92 19 72 71 40
      |29 22 86 43 12
      | 0 65 78 93 10
      |54 55 42 61 82
      |52 47 81 99 83
      |
      |81 22 90 66 82
      |92 56 63 79 32
      |72 60 30 42 20
      |91 38 10 70 13
      |46 52 47 11 69
      |
      |11 86 32 54 47
      |87 38 74 41 69
      |17 23 36 61 29
      |97 68 62 65 83
      |30  0 28 72 19
      |
      |55 65 28  7  5
      |90 93 99 48 80
      |34 94 82 19 86
      |49 39 69 75 71
      | 8 24 43 33 21
      |
      |39 70  7 56 20
      |24 67 86 45  1
      |33 44 83 76  2
      |46 78 17 94 48
      |28  4 30 77 79
      |
      |18 99 73 55 30
      |88 92 13 97  1
      |91 49 11 48 83
      |94 41  5 29 72
      |61 17 84 64 90
      |
      | 9 13 65  1 85
      |11 20 30 86 84
      |35 83 99 32 38
      |41  7  6 49 58
      |90 87 76 23 28
      |
      |89 16 91 76 78
      |29 26 27  3 90
      |42 94 43  9 57
      |59 66 80 11 24
      |31 53 75 28 20
      |
      |82 65 50 30 79
      |19 53 94 17 59
      |33 47 78 75  7
      |84 25 80 83 76
      |81 95 72 11 21
      |
      |28 26 52  5  3
      | 4 59 51 32 41
      |19 58 42 90 43
      |22 89 39 40 24
      |36 57 64 20  9
      |
      |44 65 41 79 75
      |63 76  6 51 30
      |12 21 73 29 97
      |42 55 54 53 25
      | 0 89 47 14 92
      |
      |56  4 60 63 21
      |20 50 24 77 22
      |67 66 64 91 28
      |36 57 68 87 98
      | 7 86 42 33 39
      |
      |34 15 64 46 50
      |56  7 99 69 89
      |83 23 57 13 70
      |86 71 85 36 98
      |33 76  8 54 42
      |
      |22 88 25 32 45
      | 2 21 40 11 16
      |84 37 90 27 69
      |51  1 89 49 15
      |72 96  0 65  6
      |
      |97 79 90 95  5
      |14 96 57 40 30
      |70 60 52 33 36
      |10 86 28 51  7
      |88 20 99 27 63
      |
      |84  6 57 66 62
      |56 80 97 55 58
      |92 46 81 21 26
      |99 29 27 63 87
      |39 20  7 35 48
      |
      |84 40 26  1 46
      |28 42 29  5 45
      |63 82 17 31  6
      |30 78  2 89 67
      |14 47 60 33 32
      |
      |40 89 32 50 90
      | 1  5 83 41 77
      |19 48  6 11 70
      |78 56 93 36 73
      |80  9 21 26 22
      |
      | 8 97 13  2 38
      |70 61 67 55 16
      |35 42 33  9 28
      |26 93 86  4 65
      |79 57 19 98 62
      |
      |42 91 75 97 66
      |50 12 53 52 20
      |56 70 96  5 21
      |89 57 83 18 17
      |77 72 95 38 98
      |
      |40 98 10 67 90
      |16  7 75 23 13
      |78 38 53 45 20
      | 0 28 87 94 25
      |26 83 34 56  8
      |
      |83 43 49 31 73
      |62 54 89 12 34
      |92 35 57 91 52
      |58 80 20 15 90
      |51 13 61  8 17
      |
      |19 65 73 81  5
      |57 71 52 51 22
      |48 53 15 34 66
      |63 45 96 47 49
      |58 42 56 62 76
      |
      |44  9 76 49 75
      |78 51 87 39 54
      |29 62 47 42 97
      |48 73 50 89 84
      | 0 40 38 20 81
      |
      |65 34 92 70 36
      |24 54 41 31 13
      |28 40 93 57 20
      |19 59 89 51 77
      |80 69 85 76 14
      |
      |67 78 60 98 88
      |64 46  4 84 25
      |50 87 74 56 42
      |59  0  7 31 61
      |93 12  9 33 32
      |
      |29 25 94 40 53
      |49 77 65 27 18
      | 5 92 75 90 47
      |46 16 82  1 21
      |22  3 78 13 85
      |
      |16  2 12 64 57
      |51 28 29 46 66
      |45 84 37 35 50
      |90 75 34 47 39
      |10 68  4 31  5
      |
      |30 23 47 48  7
      |73 16 71 12 25
      |91 53 43 79  0
      |81 64 35 93 37
      |83 52 87 46 85
      |
      |15 53 29  5 96
      |23 61 52 36 83
      |54 64 99 16 68
      |60 82 90 58 13
      |42 14 59 80 27
      |
      |11 54  7 24 96
      |43 32  5 95 93
      |22 49 85 64 40
      |51 18 39 47 34
      |63 21 80 75 82
      |
      |32  6 43 27 25
      | 4 20 40 59 58
      |46 47  8 65 33
      |12 21 29 84  2
      |86 30 26 62 37
      |
      |34 58 13 38 41
      |40 53 52 54 94
      |37 74 16 25 99
      |22 62 11 61 51
      |27 96  6 44  0
      |
      |68 87 53 96 90
      |17 49 45 13 93
      |21 38 62 35 27
      |56  1 65 10 33
      |16 48 22 47 67
      |
      |90 79 22 24 72
      |63 65 18 12 11
      |69 37  1 10 21
      |73 45 64  4  8
      |75 77 25 80 76
      |
      |84  6 82  5 21
      |79 62 42 78 35
      |39 41 59 65 29
      |25 54  7 31 93
      |43 86 15 61 96
      |
      | 1 80 34 86  3
      |12 49 29  7 82
      |16 70 23 45  2
      |17 75 52 28 13
      |38 25 74 77 39
      |
      |16 11 70 63 14
      |25 61 13 84 34
      |96 24 30 38 39
      |75 72 59 97 91
      | 8  4 62 19 58
      |
      | 5 66 76 33 29
      |72 92  7 87 73
      |68 94 93 60 61
      |21  3 10 20 89
      |35 47 34 48 59
      |
      |32 79 54 30 93
      |19 45  4 26 50
      |48 86 38  6 85
      |25 61 66 55 51
      |68 27 39 20  7
      |
      |40 57 61 28 85
      |54 96 20 99 69
      |83 33 91  2 93
      |92 30 53 12 16
      |35 73 58 65 98
      |
      |60  3 95 59 52
      |75 89 91 96 92
      |66  8 34 45 21
      | 6 39  2 50 55
      |19 26 86 12 94
      |
      |93 55 44 91  8
      |81 89 23 77 97
      | 2 92  6 76 39
      |21  0 56 90 51
      |16 10  5 32 66
      |
      | 4 62 54 89 43
      |75 22 13 10 68
      |91 71 69 56 96
      |55 12 53 21 39
      |19  5 51 70  3
      |
      |""".stripMargin
    }
  }
}
