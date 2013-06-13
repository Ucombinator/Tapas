package main.scala.com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.analysis.State
import com.ucombinator.dalvik.analysis.Address
import com.ucombinator.dalvik.analysis.FramePointer
import com.ucombinator.dalvik.AST.Instruction

class IntraproceduralState(statements_ : List[Instruction], framePointer_ : FramePointer, store_ : Map[Address, Any]) extends State {
  
  private val _statements: List[Instruction] = statements_
  private val _framePointer: FramePointer = framePointer_
  private val _store: Map[Address, Any] = store_
  val stackSummary: Any = None
  val assumptions: Any = None

  // C
  def statements: List[Instruction] = _statements
  def framePointer: FramePointer = _framePointer
  // S: lookup by address in store
  def lookup(address: Address): Any = {
    if (_store isDefinedAt address) {
      _store get address
    }
  }
  // In an intraprocedural analyzer, there is no stack.
  def peek: Any = _framePointer
  // K: intraprocedural, so there is no kont.
  def kont: Any = None

}