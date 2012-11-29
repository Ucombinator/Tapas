package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST.Instruction

abstract class ControlState {
  // statements have pointers to next statements
  def statement: Instruction
  def framePointer: FramePointer
  // lookup by address in store
  def lookup(address: Any): Any
  def peek: Any /* peeks at the front of the statement linked-list?? or... */
  def kont: Any /* gets continuation */
}