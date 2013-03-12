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
class ClassDef(val name: String, val accessFlags: Long, var superClass: JavaType,
  val interfaces: Array[JavaType], val sourceFile: String,
  val annotations: AnnotationsDirectoryItem, val staticFields: Array[FieldDef],
  val instanceFields: Array[FieldDef], val directMethods: Array[MethodDef],
  val virtualMethods: Array[MethodDef])
  extends JavaType {

  val visibility = if ((accessFlags & AccessFlags.ACC_PUBLIC) != 0) PublicVisibilityAttr
                   else if ((accessFlags & AccessFlags.ACC_PRIVATE) != 0) PrivateVisibilityAttr
                   else if ((accessFlags & AccessFlags.ACC_PROTECTED) != 0) ProtectedVisibilityAttr
                   else null

  val isStatic = ((accessFlags & AccessFlags.ACC_STATIC) != 0)

  val isFinal = ((accessFlags & AccessFlags.ACC_FINAL) != 0)

  val isInterface = ((accessFlags & AccessFlags.ACC_INTERFACE) != 0)

  val isAbstract = ((accessFlags & AccessFlags.ACC_ABSTRACT) != 0)

  val isSynthetic = ((accessFlags & AccessFlags.ACC_SYNTHETIC) != 0)

  val isAnnotation = ((accessFlags & AccessFlags.ACC_ANNOTATION) != 0)

  val isEnum = ((accessFlags & AccessFlags.ACC_ENUM) != 0)

  lazy val fields = staticFields ++ instanceFields

  lazy val methods = directMethods ++ virtualMethods

  lazy val methodMap = methods.foldLeft(Map.empty[String,MethodDef])((map, meth) => map + (meth.name -> meth))
  
}

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

