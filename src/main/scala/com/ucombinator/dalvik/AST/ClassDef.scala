package com.ucombinator.dalvik.AST

// Represents the interface for all Java types, whether user-defined, library-defined, or primitive
abstract class JavaType {
  
}

object VoidType extends JavaType
object BooleanType extends JavaType
object ByteType extends JavaType
object ShortType extends JavaType
object CharType extends JavaType
object IntType extends JavaType
object LongType extends JavaType
object FloatType extends JavaType
object DoubleType extends JavaType
class ArrayType(var typeOf: JavaType) extends JavaType

// This is a temporary place holder for an actual type,
// used while the file is being read.
class AbstractType(val nameOf: String) extends JavaType

// Represents the interface for all classes, whether user-defined or library-defined
//abstract class ClassDef(superclass: ClassDef, sourcefile: String) extends JavaType {
//  
//}


// A user-class defined in the program syntax
//class SyntaxClassDef(superclass: ClassDef, sourcefile: String)  extends ClassDef(superclass, sourcefile)  {
//  val fields: Map[String, FieldDef] = Map(/* TODO(petey): what goes here */)
//  
//  
//}

// TODO: sync-up this ClassDef with the older (existing) ClassDef
class ClassDef(val className:String, val accessFlags:Long, var superClass:JavaType,
  val interfaces:Array[JavaType], val sourceFile:String, val annotations:AnnotationsDirectoryItem,
  val classData:ClassData, val staticValues:Array[EncodedValue]) extends JavaType

class CodeItem(val registersSize:Int, val insSize:Int, val outsSize:Int,
  val insns:Array[Instruction], val tries:Array[Try])

class TryItem(val startAddr:Long, val insnsCount:Int, val handlerOff:Int)
class Try(val startAddr:Int, val endAddr:Int, handlers:Array[CatchHandler], val catchAllAddr:Int)

class CatchHandler(var exceptionType:JavaType, val addr:Long)
class EncodedCatchHandler(val offset:Int, val handlers:Array[CatchHandler], val catchAllAddr:Long)
 
class EncodedField(val field:Field, val accessFlags:Long)
class EncodedMethod(val method:Method, val accessFlags:Long, val code:CodeItem)

class ClassData(val staticFields:Array[EncodedField],
  val instanceFields:Array[EncodedField],
  val directMethods:Array[EncodedMethod],
  val virtualMethods:Array[EncodedMethod])

class AnnotationItem(val visibility:Short, val annotation:EncodedAnnotation)

// originally we were using type as in:
//  type AnnotationSetItem = Array[AnnotationItem]
//  type AnnotationSetRefList = Array[AnnotationSetItem]
// but that seems to make scalac unhappy when these are at the top level, so I have simply
// specified these out fully or as I might think of it in scheme---I macro expanded.


class FieldAnnotation(val field:Field, val annotations:Array[AnnotationItem])
class MethodAnnotation(val method:Method, val annotations:Array[AnnotationItem])
class ParameterAnnotation(val method:Method, val annotations:Array[Array[AnnotationItem]])

class AnnotationsDirectoryItem(val classAnnotations:Array[AnnotationItem],
  val fieldAnnotations:Array[FieldAnnotation],
  val methodAnnotations:Array[MethodAnnotation],
  val parameterAnnotations:Array[ParameterAnnotation])

/* classes to represent encoded values--probably should unencode for AST */
sealed abstract class EncodedValue
case class EncodedByte(b:Byte) extends EncodedValue
case class EncodedShort(s:Short) extends EncodedValue
case class EncodedChar(c:Char) extends EncodedValue
case class EncodedInt(i:Int) extends EncodedValue
case class EncodedLong(l:Long) extends EncodedValue
case class EncodedFloat(f:Float) extends EncodedValue
case class EncodedDouble(d:Double) extends EncodedValue
case class EncodedString(s:String) extends EncodedValue
case class EncodedType(var t:JavaType) extends EncodedValue
case class EncodedFieldVal(f:Field) extends EncodedValue
case class EncodedMethodVal(m:Method) extends EncodedValue
case class EncodedEnum(e:Field) extends EncodedValue
case class EncodedArray(vals:Array[EncodedValue]) extends EncodedValue
case class EncodedAnnotation(var javaType:JavaType, elements:Array[AnnotationElement]) extends EncodedValue
case object EncodedNull extends EncodedValue
case class EncodedBoolean(b:Boolean) extends EncodedValue
class AnnotationElement(val name:String, val value:EncodedValue)

class VarInfo(val registerNum:Long, val name:String, var varType:JavaType, val signature:String) {
  override def toString():String = {
    "{VarInfo " + registerNum + " " + name + " " + varType + 
      (if (signature == null) " " + signature else "") + "}"
  }
}
class SourceInfo(val position:Long, val line:Long, val fn:String, val varTable:Map[Long,VarInfo], val inPrologue:Boolean, val inEpilogue:Boolean) {
  override def toString():String = {
     "{SourceInfo " + position + " " + line + " " + fn + " {" + varTable.mkString(", ") + "} " + inPrologue + " " + inEpilogue + "}"
  }
}

class DebugInfo(val lineStart:Long, val parameterNames:Array[String],
  val debugTable:Map[Long,SourceInfo])

