package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST._

class ClassDefProxy(val clazz: ClassDef, var methodMap: Map[String, MethodDefProxy]) {
  private var _subclasses = Set.empty[ClassDefProxy]

  def subclasses_=(sc : Set[ClassDefProxy]) : Unit = _subclasses = sc
  def subclasses = _subclasses
}

class MethodDefProxy(val methodDef: MethodDef, val method: Method) {
  def this(methodDef: MethodDef) = this(methodDef, null)
  def this(method: Method)       = this(null, method)

  private var _calledBy = Set.empty[MethodDefProxy]
  private var _calls    = Set.empty[MethodDefProxy]

  def calls_=(callsSet: Set[MethodDefProxy]) : Unit = _calls = callsSet
  def calls = _calls
  def calledBy_=(calledBySet: Set[MethodDefProxy]) : Unit = _calledBy = calledBySet
  def calledBy = _calledBy
}

class SimpleMethodCallGraph(classes: Array[ClassDef]) {
  private def javaTypeToName(jt: JavaType) : String = {
    jt match {
      case cd: ClassDef     => cd.name
      case at: AbstractType => at.nameOf
      case _                => println("Unable to turn " + jt + " into a name") ; null
    }
  }

  private var _classMap = classes.foldLeft(Map.empty[String,ClassDefProxy]) {
    (map, clazz) =>
      map + (clazz.name ->
             new ClassDefProxy(clazz,
               clazz.methods.foldLeft(Map.empty[String,MethodDefProxy]) {
                 (map, method) => map + (method.name -> new MethodDefProxy(method))
               }))
    }

  _classMap foreach {
    (t) => {
      val clazz = t._2
      def addSubclassToClass(superClass: JavaType) : Unit = {
        superClass match {
          case cd: ClassDef     => _classMap(cd.name).subclasses += clazz
          case at: AbstractType =>
            if (_classMap isDefinedAt at.nameOf) {
              _classMap(at.nameOf).subclasses += clazz
            } else {
              val classDef = new ClassDefProxy(null, Map.empty[String, MethodDefProxy])
              classDef.subclasses += clazz
              _classMap += (at.nameOf -> classDef)
            }
          case _                => println("Unexpected type: " + superClass) 
        }
      }
      addSubclassToClass(clazz.clazz.superClass)
      if (clazz.clazz.interfaces != null) clazz.clazz.interfaces foreach addSubclassToClass
    }
  }

  classes foreach {
    (clazz) => {
      val classProxy = _classMap(clazz.name)
      clazz.methods foreach {
        (method) => {
          val methodProxy: MethodDefProxy = classProxy.methodMap(method.name)
          def addMethod(calledMethod: Method) {
            def addToClass(clazzProxy: ClassDefProxy) : Unit = {
              val mdProxy = if (clazzProxy.methodMap isDefinedAt calledMethod.name) {
                              clazzProxy.methodMap(calledMethod.name)
                            } else {
                              val mdProxy = new MethodDefProxy(calledMethod)
                              clazzProxy.methodMap += calledMethod.name -> mdProxy
                              mdProxy
                            }
              methodProxy.calls += mdProxy
              mdProxy.calledBy += methodProxy
              clazzProxy.subclasses foreach addToClass
            }
            val className = javaTypeToName(calledMethod.classType)
            val classProxy = if (_classMap isDefinedAt className) {
                               _classMap(className)
                             } else {
                               val classProxy = new ClassDefProxy(null,
                                                  Map.empty[String, MethodDefProxy])
                               _classMap += className -> classProxy
                               classProxy
                             }
            addToClass(classProxy)
          }
          if (method.code != null && method.code.insns != null) {
            method.code.insns foreach {
              (insn) => insn match {
                case InvokeSuper(args, b)          => addMethod(b)
                case InvokeDirect(args, b)         => addMethod(b)
                case InvokeStatic(args, b)         => addMethod(b)
                case InvokeInterface(args, b)      => addMethod(b)
                case InvokeVirtual(args, b)        => addMethod(b)
                case InvokeVirtualRange(c, a, b)   => addMethod(b)
                case InvokeSuperRange(c, a, b)     => addMethod(b)
                case InvokeDirectRange(c, a, b)    => addMethod(b)
                case InvokeStaticRange(c, a, b)    => addMethod(b)
                case InvokeInterfaceRange(c, a, b) => addMethod(b)
                case _                             => false
              }
            }
          }
        }
      }
    }
  }

  def classMap = _classMap
}
