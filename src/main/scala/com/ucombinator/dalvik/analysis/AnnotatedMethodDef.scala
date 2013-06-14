package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST.MethodDef
import com.ucombinator.dalvik.AST.CodeItem
import com.ucombinator.dalvik.AST.Method
import com.ucombinator.dalvik.AST.Instruction

class AnnotatedMethodDef(method: Method, accessFlags: Long, code: CodeItem) extends MethodDef(method, accessFlags, code) {
	// Apparently goto statements take Int objects as targets, so that's what we'll use
	val labelTable: Map[Int, List[Instruction]] = Map.empty
	// The idea behind using Address objects for inputs and outputs is to avoid aliasing problems
	private val inputs: Set[Address] = Set.empty
	private val outputs: Set[Address] = Set.empty
	
	private val influences: Set[(Address, Address)] = Set.empty
	
	// TODO there's probably a more colloquial way to write these functions
	def affectedSinks(input: Address): Set[Address] = influences.filter(pair => pair._1 == input).map(pair => pair._2)
	def sources(output: Address): Set[Address] = influences.filter(pair => pair._2 == output).map(pair => pair._1)
	
	// pseudo-constructor. Is there a better way to write this?
	def annotate(method: MethodDef): AnnotatedMethodDef = new AnnotatedMethodDef(method.method, method.accessFlags, method.code)
}