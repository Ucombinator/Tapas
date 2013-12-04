package org.ucombinator.dalvik.AST

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

class Field(var classType: JavaType, var fieldType: JavaType, val name: String) {
  def fullyQualifiedName = classType.toS + "." + name
}

class FieldDef(val field: Field, val accessFlags: Long, val defaultStaticValue: EncodedValue) {
  val name = field.name

  val visibility = if ((accessFlags & AccessFlags.ACC_PUBLIC) != 0) PublicVisibilityAttr
                   else if ((accessFlags & AccessFlags.ACC_PRIVATE) != 0) PrivateVisibilityAttr
                   else if ((accessFlags & AccessFlags.ACC_PROTECTED) != 0) ProtectedVisibilityAttr
                   else null

  val isStatic = ((accessFlags & AccessFlags.ACC_STATIC) != 0)

  val isFinal = ((accessFlags & AccessFlags.ACC_FINAL) != 0)

  val isVolatile = ((accessFlags & AccessFlags.ACC_VOLATILE) != 0)

  val isTransient = ((accessFlags & AccessFlags.ACC_TRANSIENT) != 0)

  val isSynthetic = ((accessFlags & AccessFlags.ACC_SYNTHETIC) != 0)

  val isEnum = ((accessFlags & AccessFlags.ACC_ENUM) != 0)

  // this is a definition, because the fieldType of a field can change from an
  // abstract type to a non-abstract type as the .dex file is read
  def fieldType: JavaType = field.fieldType
}
