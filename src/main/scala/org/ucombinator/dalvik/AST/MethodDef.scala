package org.ucombinator.dalvik.AST

class Prototype(val shortDescriptor:String, var returnType:JavaType, val parameters:Array[JavaType])

class Method(var classType:JavaType, val prototype:Prototype, val name:String) {
  private def buildTypeName(t: JavaType): String = t match {
    case VoidType         => "void"
    case BooleanType      => "boolean"
    case ByteType         => "byte"
    case ShortType        => "short"
    case CharType         => "char"
    case IntType          => "int"
    case LongType         => "long"
    case FloatType        => "float"
    case DoubleType       => "double"
    case at: AbstractType => at.nameOf
    case ar: ArrayType    => "Array[" + buildTypeName(ar.typeOf) + "]"
    case cd: ClassDef     => cd.name
    case _                => throw new Exception("Unrecognized JavaType: " + t)
  }

  def className = classType.toS

  def fullyQualifiedName = className + "." + name
}

class MethodDef(val method:Method, val accessFlags:Long, val code:CodeItem) extends Comparable[MethodDef] {
  val name = method.name

  val visibility = if ((accessFlags & AccessFlags.ACC_PUBLIC) != 0) PublicVisibilityAttr
                   else if ((accessFlags & AccessFlags.ACC_PRIVATE) != 0) PrivateVisibilityAttr
                   else if ((accessFlags & AccessFlags.ACC_PROTECTED) != 0) ProtectedVisibilityAttr
                   else null

  val isStatic = ((accessFlags & AccessFlags.ACC_STATIC) != 0)

  val isFinal = ((accessFlags & AccessFlags.ACC_FINAL) != 0)

  val isSynchronized = ((accessFlags & AccessFlags.ACC_SYNCHRONIZED) != 0)

  val isBridge = ((accessFlags & AccessFlags.ACC_BRIDGE) != 0)

  val hasVarArgs = ((accessFlags & AccessFlags.ACC_VARARGS) != 0)

  val isNative = ((accessFlags & AccessFlags.ACC_NATIVE) != 0)

  val isAbstract = ((accessFlags & AccessFlags.ACC_ABSTRACT) != 0)

  val usesStrictFP = ((accessFlags & AccessFlags.ACC_STRICT) != 0)

  val isSynthetic = ((accessFlags & AccessFlags.ACC_SYNTHETIC) != 0)

  val isConstructor = ((accessFlags & AccessFlags.ACC_CONSTRUCTOR) != 0)

  val isDeclaredSynchronized = ((accessFlags & AccessFlags.ACC_DECLARED_SYNCHRONIZED) != 0)

  // the following are methods because the returnType and parameters types can
  // change from abstract types to concrete types as the .dex file is read.

  def returnType: JavaType = method.prototype.returnType

  def parameters: Array[JavaType] = method.prototype.parameters

  def sourceLocation: Option[(String,Long,Long)] = {
    code.insns.find { (insn) => insn.sourceInfo != null } match {
      case Some(insn) => Some((insn.sourceInfo.fn, insn.sourceInfo.start_line,
          insn.sourceInfo.start_position))
      case None => None
    }
  }

  def compareTo(o: MethodDef) = {
    val fqn = method.className + "::" + name
    val ofqn = o.method.className + "::" + o.name
    fqn.compareTo(ofqn)
  }
}

class CodeItem(val registersSize:Int, val insSize:Int, val outsSize:Int,
  val insns:Array[Instruction], val tries:Array[Try])

class TryItem(val startAddr:Long, val insnsCount:Int, val handlerOff:Int)
class Try(val startAddr:Int, val endAddr:Int, handlers:Array[CatchHandler], val catchAllAddr:Int)

class CatchHandler(var exceptionType:JavaType, val addr:Long)
class EncodedCatchHandler(val offset:Int, val handlers:Array[CatchHandler], val catchAllAddr:Long)
 
