package sudoku

class Solver(val grid: Map[(Int, Int), Cell]) {

  // recursively calls the solve method until we reach a solved grid
  val s = solve(grid)
  
  type CellGroup = Map[(Int, Int), Cell]
  type CellPointer = ((Int, Int), Cell)
  
  def apply(r: Int, c:Int): Cell = s((r, c)) 
  
  def row(g: CellGroup, r: Int): CellGroup = g.filterKeys(_._1 == r)
  def col(g: CellGroup, c: Int): CellGroup = g.filterKeys(_._2 == c)
  def box(g: CellGroup, b: Int): CellGroup = {
    val x = (b - 1) / 3
    val y = (b - 1) % 3
    val topLeftRow: Int = 3 * x + 1
    val topLeftCol: Int = 3 * y + 1
    g.filterKeys(x => x._1 >= topLeftRow && x._1 < topLeftRow + 3 &&
                               x._2 >= topLeftCol && x._2 < topLeftCol + 3)
  }
  
  override def toString = {
    val x = for (r <- (1 to 9); c <- (1 to 9))
      yield {
        val sq: Cell = s(r, c)
        sq match {
          case SolvedCell(v) if (c == 1) => "\n" + v
          case SolvedCell(v) => " " + v
          case UnsolvedCell(_) if (c == 1) => "\n?"
          case _ => " ?"
        }
    }
    x.mkString
  }

  def inBox(r: Int, c: Int): Int = {
    val boxRow: Int = (r + 2) / 3
    val boxCol: Int = (c + 2) / 3
    3 * (boxRow - 1) + boxCol
  }
  
  def setCell(g: CellGroup, cp: CellPointer): CellGroup = {
    val r = cp._1._1
    val c = cp._1._2
    val v = cp._2.value 
    val b = inBox(r, c)
    
    val changedCell: CellGroup = Map(((r, c), new SolvedCell(v)))
    val newRow = purge(remainder(row(g, r), changedCell), v)
    val newCol = purge(remainder(col(g, c), changedCell), v)
    val newBox = purge(remainder(box(g, b), changedCell), v)
    g ++ changedCell ++ newRow ++ newCol ++ newBox
    
  }

  def purge(group: CellGroup, values: Set[Int]): CellGroup = 
    group map (x => (x._1, x._2.dropCandidates(values))) 
  
  def purge(group: CellGroup, value: Int): CellGroup = 
    group map (x => (x._1, x._2.dropCandidates(value)))
  
  def purgeSubset(grid: CellGroup, purgeList: (Int, CellGroup)): CellGroup = {
    val list = grid.filterKeys(a => purgeList._2.contains(a)) 
    val r = remainder(grid, list) ++ purge(list, purgeList._1)
    cleanGrid(r)
  }
  
  def unsolveds(group: CellGroup): CellGroup = group.filter(!_._2.hasValue)
  
  def solveds(group: CellGroup): CellGroup = group.filter(_._2.hasValue)

  def uniqueCandidates(group: CellGroup): Set[Int] = 
      (for (u <- unsolveds(group)) yield u._2.candidates).flatten.toSet
  
  def cleanGrid(g: CellGroup): CellGroup = {
    val sc = solveds(g)
    val gc = (g /: sc)(setCell(_, _))
    if (g == gc) gc else cleanGrid(gc) 
  }    
      
  def solve(g: CellGroup): CellGroup = {
    
    // Remove clues by iterating over the solved ones and running the purging method
    val g2 = cleanGrid(g)
    
    // Subdivide by rows, then cols, then boxes
    val g3: CellGroup = (for (i <- 1 to 9) yield subdivider(row(g2, i))).flatten.toMap
    val g3c: CellGroup = cleanGrid(g3)
    val g4: CellGroup = (for (i <- 1 to 9) yield subdivider(col(g3c, i))).flatten.toMap
    val g4c: CellGroup = cleanGrid(g4)
    val g5: CellGroup = (for (i <- 1 to 9) yield subdivider(box(g4c, i))).flatten.toMap
    val g5c: CellGroup = cleanGrid(g5)
    
    // Special sub-rows and sub-cols
    val subRows = for (r <- (1 to 9); c <- Set(1, 4, 7))
      yield g5c.filterKeys(a => (a._1 == r && a._2 >= c && a._2 <= c + 2))
    
    val subCols = for (r <- Set(1, 4, 7); c <- (1 to 9))
      yield g5c.filterKeys(a => (a._1 >= r && a._1 <= r + 2 && a._2 == c))

    val g6 = (g5c /: (subRows ++ subCols))(crossReferencer(_, _))
    val g6c: CellGroup = cleanGrid(g6)
       
    // X-wings
    val rows = (for (i <- (1 to 9)) yield unsolveds(row(g6c, i))).toSet
    val xWingRowTest = for (size <- (2 to 4); candNum <- (1 to 9); testRows <- rows.subsets(size))
                       yield (candNum, testRows, testRows.toSeq.map(a => candidateInstances(a, candNum)))
    val xWingRowResults = xWingRowTest.filter(a => a._2.size == a._3.toSet.flatten.size
                                              && a._2.size == a._3.filter(b => b.size > 1).size) 
    println(xWingRowResults)
    val purgeXWingCols = for (a <- xWingRowResults) 
                         yield (a._1, g6c.filterKeys(y => a._3.flatten.contains(y._2) 
                                                          && !a._2.map(x => x.head._1._1).contains(y._1)))
                                              
    val cols = (for (i <- (1 to 9)) yield unsolveds(col(g6c, i))).toSet
    val xWingColTest = for (size <- (2 to 4); candNum <- (1 to 9); testCols <- cols.subsets(size))
                       yield (candNum, testCols, testCols.toSeq.map(a => candidateInstances(a, candNum)))
    val xWingColResults = xWingColTest.filter(a => a._2.size == a._3.toSet.flatten.size
                                              && a._2.size == a._3.filter(b => b.size > 1).size) 
                                              
    val purgeXWingRows = for (a <- xWingColResults) 
                         yield (a._1, g6c.filterKeys(y => a._3.flatten.contains(y._1) 
                                                          && !a._2.map(x => x.head._1._2).contains(y._2)))                                              
    
    val g7 = (g6c /: (purgeXWingCols ++ purgeXWingRows))(purgeSubset(_, _))
    val g7c = cleanGrid(g7)
    
    // And round we go again
    if (g == g7c) g7c else solve(g7c)
  }
  
  // For a row test, returns the set of columns with a match (vice versa for cols)
  def candidateInstances(rc: CellGroup, candidate: Int): Set[Int] = {
    val hits = rc.filter(a => !a._2.hasValue && a._2.candidates.contains(candidate))
    hits.keys.map(a => a._2).toSet
  }  
  
  def subdivider(g: CellGroup): CellGroup = {
    
    val u = unsolveds(g)
    val c = uniqueCandidates(u)
    
    if (c == 0) g 
    else {
    
      // All combinations of unsolveds up to half the size of all unsolveds
      val subs = (for (s <- (1 to c.size / 2)) yield c.subsets(s).toSet).flatten
    
      // for each s in subs, get the matching cells from g
      val x = for (s <- subs) yield (s, matchingCells(u, s))
      
      // Filter to just those where we have a match, size-wise
      val x2 = x.filter(a => (a._1.size == a._2.size))
      
      // act = purge all other candidates not in s from matching cells, purge all 
      // candidates form s from all cells not in matching cells
      (g /: x2)(filterise(_, _))
     
    }
    
  }
  
  def filterise(g: CellGroup, s: (Set[Int], CellGroup)): CellGroup = {
    
    // Take the rest of the group, and purge it of the candidates, then take the sub-group, and purge
    // it of anything *other* than the candidates
    val r = remainder(g, s._2)
    val o = purge(r, s._1) ++ purge(s._2, (1 to 9).toSet -- s._1)
    cleanGrid(o)
    
  }
  
  // Given a set of candidates and a group of cells, return those cells that contain
  // one or more of those candidates
  def matchingCells(g: CellGroup, c: Set[Int]): CellGroup =   
      g.filter(x => !(x._2.candidates & c).isEmpty)

  // Remove one CellGroup from another
  def remainder(x: CellGroup, y: CellGroup): CellGroup = 
      x.filterKeys(a => !y.keySet.contains(a))
 
  // Give it the grid, a sub-row/col, the adjacent row/col, the adjacent box, and it will
  // remove and cross-referencing finds
  def crossReferencer(grid: CellGroup, subRowCol: CellGroup): CellGroup = {
    
    // Establish if sub-row or sub-col
    val s = subRowCol.toSeq
    val isRow = (s(1)._1._1 == s(2)._1._1)
    val lineNumber = if (isRow) s(1)._1._1 else s(1)._1._2
    
    val rowCol = if (isRow) row(grid, lineNumber) else col(grid, lineNumber)
    val adjRowCol = remainder(rowCol, subRowCol)
    val wholeBox = box(grid, inBox(s(1)._1._1, s(1)._1._2))
    val adjBox = remainder(wholeBox, subRowCol)
    
    // if sub-row/col contains a number not in adjacent row/col, purge it from adjacent box
    val purgeBoxes = for (i <- (1 to 9); if uniqueCandidates(subRowCol).contains(i) 
                                            && !uniqueCandidates(adjRowCol).contains(i)
                                            && adjRowCol.filter(a => a._2.hasValue && a._2.value == i).isEmpty) 
                     yield (i, adjBox)               
    
    // if sub-row/col contains a number not in adjacent box, purge it from adjacent row/col                                      
    val purgeRowCols = for (i <- (1 to 9); if uniqueCandidates(subRowCol).contains(i) 
                                              && !uniqueCandidates(adjBox).contains(i)
                                              && adjBox.filter(a => a._2.hasValue && a._2.value == i).isEmpty) 
                       yield (i, adjRowCol)
                        
    val allPurges = purgeBoxes ++ purgeRowCols   

    (grid /: allPurges)(purgeSubset(_, _))
    
  }
      
}
