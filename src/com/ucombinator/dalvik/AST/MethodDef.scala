package com.ucombinator.dalvik.AST

abstract case class VisibilityAttr
class PublicVisibilityAttr extends VisibilityAttr
class PrivateVisibilityAttr extends VisibilityAttr
class ProtectedVisibilityAttr extends VisibilityAttr

// Abstract classes by definition have no body, if it has no body it is abstract
// Const can be ignored
class MethodDef(name: String, args: List[JavaType], 
    visibility: VisibilityAttr, 
    isStatic: Boolean, 
    isFinal: Boolean, 
    isSynchronized: Boolean) {
  
}