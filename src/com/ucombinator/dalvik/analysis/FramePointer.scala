package com.ucombinator.dalvik.analysis

abstract class FramePointer {
  // this will probably have members at some point
  def offset(register: Any): Address
  def push: Any
}