package org.ucombinator.dalvik.AST

object AccessFlags {
  val ACC_PUBLIC = 0x1
  val ACC_PRIVATE = 0x2
  val ACC_PROTECTED = 0x4
  val ACC_STATIC = 0x8
  val ACC_FINAL = 0x10
  val ACC_SYNCHRONIZED = 0x20
  val ACC_VOLATILE = 0x40
  val ACC_BRIDGE = 0x40
  val ACC_TRANSIENT = 0x80
  val ACC_VARARGS = 0x80 
  val ACC_NATIVE = 0x100 
  val ACC_INTERFACE = 0x200
  val ACC_ABSTRACT = 0x400
  val ACC_STRICT = 0x800 
  val ACC_SYNTHETIC = 0x1000
  val ACC_ANNOTATION = 0x2000
  val ACC_ENUM = 0x4000
  val ACC_CONSTRUCTOR = 0x10000
  val ACC_DECLARED_SYNCHRONIZED = 0x20000
}

abstract sealed class VisibilityAttr
case object PublicVisibilityAttr extends VisibilityAttr
case object PrivateVisibilityAttr extends VisibilityAttr
case object ProtectedVisibilityAttr extends VisibilityAttr
