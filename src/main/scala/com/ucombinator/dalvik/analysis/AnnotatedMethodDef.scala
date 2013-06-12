package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST.MethodDef
import com.ucombinator.dalvik.AST.CodeItem
import com.ucombinator.dalvik.AST.Method
import com.ucombinator.dalvik.AST.Instruction

class AnnotatedMethodDef(method: Method, accessFlags: Long, code: CodeItem) extends MethodDef(method, accessFlags, code) {
	// Apparently goto statements take Ints, so that's what we'll use
	val labelTable: Map[Int, List[Instruction]] = Map.empty
	// TODO: Think about how to represent inputs
	private val inputs: Set[Int] = Set.empty
	// TODO: outputs probably get the same representation
	private val outputs: Set[Int] = Set.empty
	
	// TODO: define this function. Also, the Int objects should probably not be Ints.
	def influence(input: Int): Set[Int] = Set.empty
	
	// pseudo-constructor. Is there a better way to write this?
	def annotate(method: MethodDef): AnnotatedMethodDef = new AnnotatedMethodDef(method.method, method.accessFlags, method.code)
}