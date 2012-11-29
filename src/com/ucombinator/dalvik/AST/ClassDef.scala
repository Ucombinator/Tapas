package com.ucombinator.dalvik.AST

// Represents the interface for all Java types, whether user-defined, library-defined, or primitive
abstract class JavaType {
  
}


// Represents the interface for all classes, whether user-defined or library-defined
abstract class ClassDef(superclass: ClassDef, sourcefile: String) extends JavaType {
  
}


// A user-class defined in the program syntax
class SyntaxClassDef(superclass: ClassDef, sourcefile: String)  extends ClassDef(superclass, sourcefile)  {
  val fields: Map[String, FieldDef] = Map(/* TODO(petey): what goes here */)
  
  
}