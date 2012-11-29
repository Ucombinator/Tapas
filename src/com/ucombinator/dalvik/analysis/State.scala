package com.ucombinator.dalvik.analysis

//import com.ucombinator.dalvik.analysis.AbstractCounterInt

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
  val controlState: ControlState
  // PDCFA stack, not continuation
  val stackSummary: Any /* TODO: replace */
  val assumptions: Any /* TODO: replace */
  // TODO change the implementation of Map to something specific
  val counter: Map[Address, AbstractCounterInt] = Map()
}