package sudoku

object Main extends App {
  
  val clues = Map(
                  ((1, 4), new SolvedCell(1)), 
                  ((1, 6), new SolvedCell(6)),
				  ((2, 3), new SolvedCell(4)),
				  ((2, 7), new SolvedCell(3)),
				  ((3, 2), new SolvedCell(9)),
				  ((3, 4), new SolvedCell(8)),
				  ((3, 6), new SolvedCell(2)),
				  ((3, 8), new SolvedCell(5)),
				  ((4, 1), new SolvedCell(2)),
				  ((4, 3), new SolvedCell(5)),
				  ((4, 7), new SolvedCell(7)),
				  ((4, 9), new SolvedCell(9)),
				  ((6, 1), new SolvedCell(3)),
				  ((6, 3), new SolvedCell(8)),
				  ((6, 7), new SolvedCell(1)),
				  ((6, 9), new SolvedCell(6)),
				  ((7, 2), new SolvedCell(1)),
				  ((7, 4), new SolvedCell(4)),
				  ((7, 6), new SolvedCell(5)),
				  ((7, 8), new SolvedCell(2)),
				  ((8, 3), new SolvedCell(2)),
				  ((8, 7), new SolvedCell(5)),
				  ((9, 4), new SolvedCell(6)),
				  ((9, 6), new SolvedCell(8))
				 ) 
                 
  val blanks = (for (r <- (1 to 9); c <- (1 to 9); if (!clues.contains((r, c)))) 
               yield (r, c) -> new UnsolvedCell()).toMap
                 
  val s = new Solver(clues ++ blanks)
    
  println(s)
      
}