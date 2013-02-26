package com.ucombinator.dalvik.AST

// TODO(petey): rewrite this with a case object or something
abstract case class VisibilityAttr()
class PublicVisibilityAttr extends VisibilityAttr
class PrivateVisibilityAttr extends VisibilityAttr
class ProtectedVisibilityAttr extends VisibilityAttr

// Abstract classes by definition have no body, if it has no body it is abstract
// Const can be ignored
class MethodDef(name: String, args: List[String], 
    visibility: VisibilityAttr, 
    isStatic: Boolean, 
    isFinal: Boolean, 
    isSynchronized: Boolean) {
  
}

// TODO: need to get things linked up so that Method == MethodDef
class Prototype(val shortDescriptor:String, var returnType:JavaType, val parameters:Array[JavaType])
class Method(var classType:JavaType, val prototype:Prototype, val name:String)

