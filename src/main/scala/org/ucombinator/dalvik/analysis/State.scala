package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST.Instruction

sealed class AbstractCounterInt {
}

case object Zero extends AbstractCounterInt {
  
}

case object One extends AbstractCounterInt {
  
}

case object Unbounded extends AbstractCounterInt {
  
}

// (control-state x stack-summary x proposition* x counter)
abstract class State {
  // C: statements have pointers to next statements
  def statement: Instruction
  def framePointer: FramePointer
  // S: lookup by address in store
  def lookup(address: Address): Any
  def peek: Any /* gets the top of the PDCFA stack */
  // K: get continuation
  def kont: Any
  // PDCFA stack, not continuation
  val stackSummary: Any /* TODO: replace */
  val assumptions: Any /* TODO: replace */
  // TODO change the implementation of Map to something specific
  val counter: Map[Address, AbstractCounterInt] = Map()
}