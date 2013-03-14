package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST._

/** Class for performing abstract evaluation on a single method.
  *
  * @constructor creates a new method abstract evaluator for the specified
  *              method of the specified class.
  * @param clazz the class containing the method.
  * @param method the method defintion to be analyzed.
  */
class MethodAbstractEval(clazz: ClassDef, method: MethodDef) {
  
  /** A simple method to process the bytecode of a method looking for the set
    * of methods that can be called from this method.
    *
    * This procedure does not perform any abstract evaluation, it is here
    * mostly to serve as a first pass approximation of what we want this
    * analzyer to do.
    *
    * @return returns the set of method definitions that could be called here.
    */
  def gatherCalledMethods: Set[Method] = {
    method.code.insns.foldLeft(Set.empty[Method]) {
      (set, insn) => insn match {
        case InvokeSuper(args, b)          => set + b
        case InvokeDirect(args, b)         => set + b
        case InvokeStatic(args, b)         => set + b
        case InvokeInterface(args, b)      => set + b
        case InvokeVirtual(args, b)        => set + b
        case InvokeVirtualRange(c, a, b)   => set + b
        case InvokeSuperRange(c, a, b)     => set + b
        case InvokeDirectRange(c, a, b)    => set + b
        case InvokeStaticRange(c, a, b)    => set + b
        case InvokeInterfaceRange(c, a, b) => set + b
        case _                             => set
      }
    }
  }
}

class MethodCallAnalyzer(clazzes: Array[ClassDef]) {
  var classMap = Map.empty[String,Map[String,Option[Set[Method]]]]

  private def doAnalysis(className: String, methodName: String): Option[Set[Method]] = {
    clazzes find { (cd) => cd.name == className } match {
      case Some(classDef) =>
        classDef.methods find { (md) => md.name == methodName } match {
          case Some(methodDef) => {
            val evaluator = new MethodAbstractEval(classDef, methodDef)
            Some(evaluator.gatherCalledMethods)
          }
          case None => None
        }
      case None => None
    }
  }
  
  def lookupMethods(className: String, methodName: String): Option[Set[Method]] = {
    if (classMap isDefinedAt className) {
      val methodMap = classMap(className)
      if (methodMap isDefinedAt methodName) {
        methodMap(methodName)
      } else {
        val methods = doAnalysis(className, methodName)
        classMap = classMap.updated(className, classMap(className) + (methodName -> methods))
        methods
      }
    } else {
      val methods = doAnalysis(className, methodName)
      classMap += className -> Map(methodName -> methods)
      methods
    }
  }
}
