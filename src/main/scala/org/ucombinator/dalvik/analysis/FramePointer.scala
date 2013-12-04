package org.ucombinator.dalvik.analysis

abstract class FramePointer {
  // this will probably have members at some point
  def offset(register: Any): Address
  // TODO(matt): shouldn't this take an argument or two? Like a register or a register and a value?
  def push: Any
}
