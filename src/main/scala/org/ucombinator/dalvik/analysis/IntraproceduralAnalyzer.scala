package org.ucombinator.dalvik.analysis

import org.ucombinator.dalvik.AST._

class IntraproceduralAnalyzer(classes: Array[ClassDef]) extends SimpleMethodCallGraph(classes: Array[ClassDef]) {
  override def processInstructions(methodProxy: MethodDefProxy)(code: CodeItem) {
    val add = addMethod(methodProxy)_
    code.insns foreach {
      (insn) => insn match {
        case _ => false
      }
    }
  }
}
