package sudoku

abstract class Cell { 
  val hasValue: Boolean
  val value: Int
  val candidates: Set[Int]
  def dropCandidates(badCandidate: Int): Cell
  def dropCandidates(badCandidates: Set[Int]): Cell
}

case class SolvedCell(val value: Int) extends Cell {
  val hasValue = true
  lazy val candidates = throw new Error("No candidates")
  def dropCandidates(badCandidate: Int): Cell = this
  def dropCandidates(badCandidates: Set[Int]): Cell = this
  
  if (value < 1 || value > 9) throw new Error("Not valid value")
  
  override def toString = "[" + value + "]"
  
}

case class UnsolvedCell(val candidates: Set[Int]) extends Cell {
  
  def this() = this((1 to 9).toSet)  // Auxiliary constructor
  
  val hasValue = false
  lazy val value = throw new Error("No value")
  
  if (candidates.isEmpty) throw new Error("Empty candidate set")
  else if (!candidates.filter(c => c < 1 || c > 9).isEmpty) throw new Error("Candidates out of range")
  
  def setValue(value: Int): Cell = new SolvedCell(value)
  
  def dropCandidates(badCandidate: Int): Cell = dropCandidates(Set(badCandidate))
  
  def dropCandidates(badCandidates: Set[Int]): Cell = {
    val remainder: Set[Int] = this.candidates diff badCandidates
        
    if (remainder.size == 1) new SolvedCell(remainder.head)
    else new UnsolvedCell(remainder)
  }
  
  override def toString = "{" + candidates.toString + "}"
  
}

 