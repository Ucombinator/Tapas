package com.ucombinator.dalvik.AST

import annotation.tailrec
import language.implicitConversions

// http://www.milk.com/kodebase/dalvik-docs-mirror/docs/dalvik-bytecode.html

class VarInfo(val registerNum: Long, val name: String, var varType: JavaType,
  val signature: String)

class SourceInfo(val position: Long, val line: Long, val fn: String,
  val varTable: Map[Long,VarInfo], val inPrologue: Boolean,
  val inEpilogue: Boolean)

class DebugInfo(val lineStart: Long, val parameterNames: Array[String],
  val debugTable: Map[Long,SourceInfo])



abstract class Instruction {
  var sourceInfo: SourceInfo = null

  protected def getInstructionName: String = {
    val name = this.getClass.getSimpleName
    val len = name.length
    @tailrec
    def convert(i: Int, addedSlash: Boolean, str: String): String = {
      if (i == len) {
        str
      } else {
        val ch = name(i)
        if (ch == 'F' && i+4 <= len && name.substring(i,i+4) == "From") {
          convert(i + 4, true, str + "/from")
        } else if (ch == 'R' && i+5 <= len && name.substring(i,i+5) == "Range") {
          convert(i + 5, true, str + "/range")
        } else if (ch == 'H' && i+4 <= len && name.substring(i,i+4) == "High") {
          convert(i + 4, true, str + "/high")
        } else if (ch == 'J' && i+5 <= len && name.substring(i,i+5) == "Jumbo") {
          convert(i + 5, true, str + "/jumbo")
        } else if (ch == 'L' && i+3 <= len && name.substring(i,i+3) == "Lit") {
          convert(i + 3, true, str + "/lit")
        } else if (ch.isDigit && !addedSlash) {
          convert(i+1, true, str + "/" + ch)
        } else if (ch.isUpper) {
          convert(i + 1, addedSlash,
              (if (i == 0) str + ch.toLower else str + "-" + ch.toLower))
        } else if (ch == '$') {
          convert(i+1, addedSlash, str)
        } else {
          convert(i+1, addedSlash, str + ch)
        }
      }
    }
    convert(0,false,"")
  }

  protected def getParameters: Array[(String, String, Object)] =
    this.getClass.getDeclaredFields map { (fld) =>
      fld.setAccessible(true)
      (fld.getType.getSimpleName, fld.getName, fld.get(this))
    }

  protected def convertParameter(info: (String, String, Object)): String = {
    info._1 match {
      case "byte" | "short" | "int" | "long" => "v" + info._3
      case "byte[]" => "{v" + info._3.asInstanceOf[Array[Byte]].mkString(", v") + "}"
      case "int[]" => "{" + info._3.asInstanceOf[Array[Int]].mkString(", ") + "}"
      case "short[]" => "{" + info._3.asInstanceOf[Array[Short]].mkString(", ") + "}"
      case "String" => "\"" + info._3.toString + "\""
      case "Method" => info._3.asInstanceOf[Method].fullyQualifiedName
      case "JavaType" => info._3.asInstanceOf[JavaType].toS
      case "Field" => info._3.asInstanceOf[Field].fullyQualifiedName
      case _ => info.toString
    }
  }

  protected def rangeToVarRef(A:Short, C:Int):String = {
    val N = A + C - 1
    if (C == N) "{v" + C + "}" else "{v" + C + " .. v" + N + "}"
  }

  protected def toSInvokeRanged(): String = {
    val instrName = getInstructionName
    val fields = getParameters
    instrName + " " + rangeToVarRef(fields(1)._3.toString.toShort, fields(0)._3.toString.toInt) +
      " " + convertParameter(fields(2))
  }

  protected def toSLastRaw(): String = {
    val instrName = getInstructionName
    val params = getParameters
    val fields = params.take(params.length - 1)
    val last = params.last
    instrName + " " + fields.map(convertParameter).toString + ", " + last._3
  }

  def toS(): String = {
    val instrName = getInstructionName
    val fields = getParameters.map(convertParameter)
    instrName + " " + fields.mkString(", ")
  }
}

case object End extends Instruction { override def toS = "end" }
case object Nop extends Instruction { override def toS = "nop" }

// TODO(petey): needs parameters (AWK: actually if we are using case classes,
// it is easier not to have parameters here)
// NOTE(awk): removing the hierarchy at the recommendation of scalac, as
//            inheriting from case-classes can lead to bugs, according to the
//            scalac deprecation warnings
// abstract case class Instruction extends Instruction

// wide instructions get expanded to two adjacent instructions (AWK: not sure what this means, or do you want to model a single Wide move as two normal moves?)
// NOTE(awk): changed the names to reflect the names used in the Dalvik
//            bytecode
// NOTE(awk): the following could probably be safely collapsed for us,
//            if we don't care about distinguishing a 4-bit register address
//            from an 8-bit or 16-bit regsiter address.

// moves a value from one register to another, where both registers are
// specified with a 4-bit register address
case class Move(a:Byte, b:Byte) extends Instruction
case class MoveWide(a:Byte, b:Byte) extends Instruction
case class MoveObject(a:Byte, b:Byte) extends Instruction

// moves the result of an invoke instruction to a register (or register pair)
// with an 8-bit register address
case class MoveResult(a:Short) extends Instruction
case class MoveResultWide(a:Short) extends Instruction
case class MoveResultObject(a:Short) extends Instruction
case class MoveException(a:Short) extends Instruction

// moves from a register with a 16-bit register address to a register with an
// 8-bit register address
case class MoveFrom16(a:Short, b:Int) extends Instruction
case class MoveWideFrom16(a:Short, b:Int) extends Instruction
case class MoveObjectFrom16(a:Short, b:Int) extends Instruction

// moves from a register with a 16-bit register address to another register
// with a 16-bit register address
case class Move16(a:Int, b:Int) extends Instruction
case class MoveWide16(a:Int, b:Int) extends Instruction
case class MoveObject16(a:Int, b:Int) extends Instruction

// return a void value
case object ReturnVoid extends Instruction {
  override def getInstructionName = "return-void"
}

// return a value from a register specified by a 16-bit register address
case class Return(a:Short) extends Instruction
case class ReturnWide(a:Short) extends Instruction
case class ReturnObject(a:Short) extends Instruction

// instructions for invoking methods

// Invoke the method indicated by B with the argument registers at the 4-bit
// register addresses referenced from the args array
case class InvokeSuper(args:Array[Byte], b:Method) extends Instruction
case class InvokeDirect(args:Array[Byte], b:Method) extends Instruction
case class InvokeStatic(args:Array[Byte], b:Method) extends Instruction
case class InvokeInterface(args:Array[Byte], b:Method) extends Instruction
case class InvokeVirtual(args:Array[Byte], b:Method) extends Instruction

// Invoke the method indicated by B with the argument registers starting at the
// 16-bit address referenced by C and until a total of A arguments are used
case class InvokeVirtualRange(c:Int, a:Short, b:Method) extends Instruction {
  override def toS(): String = toSInvokeRanged()
}
case class InvokeSuperRange(c:Int, a:Short, b:Method) extends Instruction {
  override def toS(): String = toSInvokeRanged()
}
case class InvokeDirectRange(c:Int, a:Short, b:Method) extends Instruction {
  override def toS(): String = toSInvokeRanged()
}
case class InvokeStaticRange(c:Int, a:Short, b:Method) extends Instruction {
  override def toS(): String = toSInvokeRanged()
}
case class InvokeInterfaceRange(c:Int, a:Short, b:Method) extends Instruction {
  override def toS(): String = toSInvokeRanged()
}

// instructions for branching

// Jump, unconditionally, to the given target
case class Goto(a:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class Goto16(a:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class Goto32(a:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// Jump to the relative target indicated in B when the value at the 8-bit
// register address in A is related to 0
case class IfEqz(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfNez(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfLtz(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfGez(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfGtz(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfLez(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// Jump to the relative target indicated in C when the values at the 4-bit
// register address in A and B satisfy the given relationship
case class IfEq(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfNe(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfLt(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfGe(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfGt(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class IfLe(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// Swithcing statements to support jump tables
// TODO: Need to supply the packed or spaces swithc payload to
// Jumps based on the value in the 8-bit register address referenced by A
case class TempPackedSwitch(a:Short, b:Int) extends Instruction {
  override def getInstructionName = "packed-switch"
  override def toS(): String = toSLastRaw()
}
case class TempSparseSwitch(a:Short, b:Int) extends Instruction {
  override def getInstructionName = "sparse-switch"
  override def toS(): String = toSLastRaw()
}
case class PackedSwitch(a:Short, firstKey:Int, targets:Array[Int]) extends Instruction {
  override def toS(): String =
    "packed-switch v" + a + ", " + firstKey + ", {" + targets.mkString(", ") + "}"
}
case class SparseSwitch(a:Short, keys:Array[Int], targets:Array[Int]) extends Instruction {
  override def toS(): String =
    "sparse-switch v" + a + " {" + keys.zip(targets).mkString(", ") + "}"
}

// throw the exception indicated in A
case class Throw(a:Short) extends Instruction

// instructions for entering and leaving monitors

// Enters or exits the the monitor indcated by the 8-bit register address in A
case class MonitorEnter(a:Short) extends Instruction
case class MonitorExit(a:Short) extends Instruction


// Comparison operators for floats, doubles, and long

// Compares the values from the 8-bit register addresses in B and C and writes
// the result to the 8-bit register address in A, where the result is
// 0 if [B] == [C], -1 if [B] < [C], or 1 if [B] > [C]
case class CmplFloat(a:Short, b:Short, c:Short) extends Instruction
case class CmpgFloat(a:Short, b:Short, c:Short) extends Instruction
case class CmplDouble(a:Short, b:Short, c:Short) extends Instruction
case class CmpgDouble(a:Short, b:Short, c:Short) extends Instruction
case class CmpLong(a:Short, b:Short, c:Short) extends Instruction


// instructions for loading constants

// stores the 32-bit (sign-extended if necessary) constant in B into a register
// with a 4-bit (Const4) or an 8-bit (Const16, Const) register address
case class Const4(a:Byte, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class Const16(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class Const(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// stores the 32-bit (right-zero-extended) constant in B into a register with
// an 8-bit register address
case class ConstHigh16(a:Short, b:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// stores the 64-bit (sign-extended if necessary) constant in B into a
// register-pair with an 8-bit register address
case class ConstWide16(a:Short, b:Long) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class ConstWide32(a:Short, b:Long) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class ConstWide(a:Short, b:Long) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// stores the 64-bit (right-zero-extended) constant in B into a register with
// an 8-bit register address
case class ConstWideHigh16(a:Short, b:Long) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// stores a String from the String constants table into a register with an
// 8-bit register address
case class ConstString(a:Short, b:String) extends Instruction
case class ConstStringJumbo(a:Short, b:String) extends Instruction

// stores the class (type) indicated by the associated into a register with an
// 8-bit register address
case class ConstClass(a:Short, var b:JavaType) extends Instruction

// Unary opertators

// Performs negation on the value in the register specified by the 4-bit
// register address in B and puts the value at the 4-bit register address in A
case class NegInt(a:Byte, b:Byte) extends Instruction
case class NegLong(a:Byte, b:Byte) extends Instruction
case class NegFloat(a:Byte, b:Byte) extends Instruction
case class NegDouble(a:Byte, b:Byte) extends Instruction

// Performs logical negation on the value in the register specified by the
// 4-bit register address in B and puts the value at the 4-bit register address
// in A
case class NotInt(a:Byte, b:Byte) extends Instruction
case class NotLong(a:Byte, b:Byte) extends Instruction

// Conversion operators

// convert value found at 4-bit register address in B into the appropriate type
// of value and store it in the 4-bit register address in A
case class IntToLong(a:Byte, b:Byte) extends Instruction
case class IntToFloat(a:Byte, b:Byte) extends Instruction
case class IntToDouble(a:Byte, b:Byte) extends Instruction
case class LongToInt(a:Byte, b:Byte) extends Instruction
case class LongToFloat(a:Byte, b:Byte) extends Instruction
case class LongToDouble(a:Byte, b:Byte) extends Instruction
case class FloatToInt(a:Byte, b:Byte) extends Instruction
case class FloatToLong(a:Byte, b:Byte) extends Instruction
case class FloatToDouble(a:Byte, b:Byte) extends Instruction
case class DoubleToInt(a:Byte, b:Byte) extends Instruction
case class DoubleToLong(a:Byte, b:Byte) extends Instruction
case class DoubleToFloat(a:Byte, b:Byte) extends Instruction
case class IntToByte(a:Byte, b:Byte) extends Instruction
case class IntToChar(a:Byte, b:Byte) extends Instruction
case class IntToShort(a:Byte, b:Byte) extends Instruction

// Arithmetic two address instructions

// perform arithmetic operation on values found a A and B 4-bit register
// address, and write result into A regster
case class AddInt2Addr(a:Byte, b:Byte) extends Instruction
case class SubInt2Addr(a:Byte, b:Byte) extends Instruction
case class MulInt2Addr(a:Byte, b:Byte) extends Instruction
case class DivInt2Addr(a:Byte, b:Byte) extends Instruction
case class RemInt2Addr(a:Byte, b:Byte) extends Instruction
case class AndInt2Addr(a:Byte, b:Byte) extends Instruction
case class OrInt2Addr(a:Byte, b:Byte) extends Instruction
case class XorInt2Addr(a:Byte, b:Byte) extends Instruction
case class ShlInt2Addr(a:Byte, b:Byte) extends Instruction
case class ShrInt2Addr(a:Byte, b:Byte) extends Instruction
case class UshrInt2Addr(a:Byte, b:Byte) extends Instruction
case class AddLong2Addr(a:Byte, b:Byte) extends Instruction
case class SubLong2Addr(a:Byte, b:Byte) extends Instruction
case class MulLong2Addr(a:Byte, b:Byte) extends Instruction
case class DivLong2Addr(a:Byte, b:Byte) extends Instruction
case class RemLong2Addr(a:Byte, b:Byte) extends Instruction
case class AndLong2Addr(a:Byte, b:Byte) extends Instruction
case class OrLong2Addr(a:Byte, b:Byte) extends Instruction
case class XorLong2Addr(a:Byte, b:Byte) extends Instruction
case class ShlLong2Addr(a:Byte, b:Byte) extends Instruction
case class ShrLong2Addr(a:Byte, b:Byte) extends Instruction
case class UshrLong2Addr(a:Byte, b:Byte) extends Instruction
case class AddFloat2Addr(a:Byte, b:Byte) extends Instruction
case class SubFloat2Addr(a:Byte, b:Byte) extends Instruction
case class MulFloat2Addr(a:Byte, b:Byte) extends Instruction
case class DivFloat2Addr(a:Byte, b:Byte) extends Instruction
case class RemFloat2Addr(a:Byte, b:Byte) extends Instruction
case class AddDouble2Addr(a:Byte, b:Byte) extends Instruction
case class SubDouble2Addr(a:Byte, b:Byte) extends Instruction
case class MulDouble2Addr(a:Byte, b:Byte) extends Instruction
case class DivDouble2Addr(a:Byte, b:Byte) extends Instruction
case class RemDouble2Addr(a:Byte, b:Byte) extends Instruction

// Arithmetic three address instructions

// perform arithmetic opertion on values found at B and C 8-bit register
// addresses and write result into A
case class AddInt(a:Short, b:Short, c:Short) extends Instruction
case class SubInt(a:Short, b:Short, c:Short) extends Instruction
case class MulInt(a:Short, b:Short, c:Short) extends Instruction
case class DivInt(a:Short, b:Short, c:Short) extends Instruction
case class RemInt(a:Short, b:Short, c:Short) extends Instruction
case class AndInt(a:Short, b:Short, c:Short) extends Instruction
case class OrInt(a:Short, b:Short, c:Short) extends Instruction
case class XorInt(a:Short, b:Short, c:Short) extends Instruction
case class ShlInt(a:Short, b:Short, c:Short) extends Instruction
case class ShrInt(a:Short, b:Short, c:Short) extends Instruction
case class UshrInt(a:Short, b:Short, c:Short) extends Instruction
case class AddLong(a:Short, b:Short, c:Short) extends Instruction
case class SubLong(a:Short, b:Short, c:Short) extends Instruction
case class MulLong(a:Short, b:Short, c:Short) extends Instruction
case class DivLong(a:Short, b:Short, c:Short) extends Instruction
case class RemLong(a:Short, b:Short, c:Short) extends Instruction
case class AndLong(a:Short, b:Short, c:Short) extends Instruction
case class OrLong(a:Short, b:Short, c:Short) extends Instruction
case class XorLong(a:Short, b:Short, c:Short) extends Instruction
case class ShlLong(a:Short, b:Short, c:Short) extends Instruction
case class ShrLong(a:Short, b:Short, c:Short) extends Instruction
case class UshrLong(a:Short, b:Short, c:Short) extends Instruction
case class AddFloat(a:Short, b:Short, c:Short) extends Instruction
case class SubFloat(a:Short, b:Short, c:Short) extends Instruction
case class MulFloat(a:Short, b:Short, c:Short) extends Instruction
case class DivFloat(a:Short, b:Short, c:Short) extends Instruction
case class RemFloat(a:Short, b:Short, c:Short) extends Instruction
case class AddDouble(a:Short, b:Short, c:Short) extends Instruction
case class SubDouble(a:Short, b:Short, c:Short) extends Instruction
case class MulDouble(a:Short, b:Short, c:Short) extends Instruction
case class DivDouble(a:Short, b:Short, c:Short) extends Instruction
case class RemDouble(a:Short, b:Short, c:Short) extends Instruction

// Arithmetic with 32-bit (sign extended from 8-bit) literal integer

// perform arithmetic operation on value found at B 8-bit register and inline
// literal and put result in A
case class AddIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class RsubIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class MulIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class DivIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class RemIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class AndIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class OrIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class XorIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class ShlIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class ShrIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class UshrIntLit8(a:Short, b:Short, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// Arithmetic with 32-bit (sign extended from 16-bit) literal integer

// perform arithmetic operation on value found at B 4-bit register and the
// inline literal and put result in A
case class AddIntLit16(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class RsubInt(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class MulIntLit16(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class DivIntLit16(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class RemIntLit16(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class AndIntLit16(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class OrIntLit16(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}
case class XorIntLit16(a:Byte, b:Byte, c:Int) extends Instruction {
  override def toS(): String = toSLastRaw()
}

// Instructions for getting and setting instance variables on objects

// Check that the value referenced from the 8-bit register address in A can be
// cast to the type in B
case class CheckCast(a:Short, var b:JavaType) extends Instruction

// Create a new instance of the class indicated in B, and put the reslt in the
// 8-bit register address in A
case class NewInstance(a:Short, var b:JavaType) extends Instruction

// Determines if the object referenced from the 4-bit register address in B is
// of the class type referenced in C and puts the result in A, where the result
// is 1 when [B] is an instance of [C] and 0 in any other case
case class InstanceOf(a:Byte, b:Byte, var c:JavaType) extends Instruction

// Retrieves a value from the field specified in C of the object referenced
// from the 4-bit register address in B and puts the result in the register or
// register-pair addressed by the 4-bit address in A.
case class IGet(a:Byte, b:Byte, c:Field) extends Instruction
case class IGetWide(a:Byte, b:Byte, c:Field) extends Instruction
case class IGetObject(a:Byte, b:Byte, c:Field) extends Instruction
case class IGetBoolean(a:Byte, b:Byte, c:Field) extends Instruction
case class IGetByte(a:Byte, b:Byte, c:Field) extends Instruction
case class IGetChar(a:Byte, b:Byte, c:Field) extends Instruction
case class IGetShort(a:Byte, b:Byte, c:Field) extends Instruction

// Puts the value reverenced from the 4-bit register address in A into the the
// field specified in C of the object referenced from the 4-bit register
// address in B
case class IPut(a:Byte, b:Byte, c:Field) extends Instruction
case class IPutWide(a:Byte, b:Byte, c:Field) extends Instruction
case class IPutObject(a:Byte, b:Byte, c:Field) extends Instruction
case class IPutBoolean(a:Byte, b:Byte, c:Field) extends Instruction
case class IPutByte(a:Byte, b:Byte, c:Field) extends Instruction
case class IPutChar(a:Byte, b:Byte, c:Field) extends Instruction
case class IPutShort(a:Byte, b:Byte, c:Field) extends Instruction

// Instructions for getting and setting static variables on classes

// Retrieves a value from the class field specified in B and puts it in the
// register or register-pair referenced by the 8-bit register address in A
case class SGet(a:Short, b:Field) extends Instruction
case class SGetWide(a:Short, b:Field) extends Instruction
case class SGetObject(a:Short, b:Field) extends Instruction
case class SGetBoolean(a:Short, b:Field) extends Instruction
case class SGetByte(a:Short, b:Field) extends Instruction
case class SGetChar(a:Short, b:Field) extends Instruction
case class SGetShort(a:Short, b:Field) extends Instruction

// Puts a value from the register or register-pair referenced by the 8-bit
// register address in A into the class field specified in B 
case class SPut(a:Short, b:Field) extends Instruction
case class SPutWide(a:Short, b:Field) extends Instruction
case class SPutObject(a:Short, b:Field) extends Instruction
case class SPutBoolean(a:Short, b:Field) extends Instruction
case class SPutByte(a:Short, b:Field) extends Instruction
case class SPutChar(a:Short, b:Field) extends Instruction
case class SPutShort(a:Short, b:Field) extends Instruction

// Instructions for interacting with arrays

// Determines the length of the array referenced from the 4-bit register
// address in B and put the result at the 4-bit register address in A
case class ArrayLength(a:Byte, b:Byte) extends Instruction

// Creates a new Array of type C of the size stored in the 4-bit register in B
// and put the result in the 4-bit register refrenced from A
case class NewArray(a:Byte, b:Byte, var c:JavaType)  extends Instruction

// TODO: need to supply the FillArrayDataPayload, indicated in the B
// Stores the array values found in a fill-array-data-payload into the array
// referenced from the 8-bit register address in A
case class TempFillArrayData(a:Short, b:Int) extends Instruction {
  override def getInstructionName = "fill-array-data"
  override def toS(): String = toSLastRaw()
}
case class FillArrayData(a:Short, size:Long, elementWidth:Int, data:Array[Short]) extends Instruction {
  override def toS(): String =
    "fill-array-data v" + a + ", " + size + ", " + elementWidth + ", {" +
      data.mkString(", ") + "}"
}

// Creates a new filled array of the type in B with the values from the 4-bit
// register addresses referenced by the args and places the result in the
// same place as the result of calling an invoke instruction;  the result
// can be moved using an immediately subsequent move-result-object instruction
case class FilledNewArray(args:Array[Byte], var b:JavaType) extends Instruction {
  override def toS(): String =
    "fill-new-array {v" + args.mkString(", v") + "} " + b.toS
}

// Creates a new filled array of the type in B with the values from the set
// of registers starting at the 16-bit register address indicated by C for the
// count stored in A and places the result in the same place as the result of
// calling an invoke instruction;  the result can be moved using an immediately
// subsequent move-result-object instruction
case class FilledNewArrayRange(c:Int, a:Short, var b:JavaType) extends Instruction {
  override def toS(): String =
    "fill-new-array/range {v" + rangeToVarRef(a, c) + "} " + b.toS
}

// Retrieves a value from the index referenced in the 8-bit register
// address in C in an array referenced by the 8-bit register address in B and
// puts it into the 8-bit register address in A
case class AGet(a:Short, b:Short, c:Short) extends Instruction
case class AGetWide(a:Short, b:Short, c:Short) extends Instruction
case class AGetObject(a:Short, b:Short, c:Short) extends Instruction
case class AGetBoolean(a:Short, b:Short, c:Short) extends Instruction
case class AGetByte(a:Short, b:Short, c:Short) extends Instruction
case class AGetChar(a:Short, b:Short, c:Short) extends Instruction
case class AGetShort(a:Short, b:Short, c:Short) extends Instruction

// Puts a value from the 8-bit register address in A into the index referenced
// in the 8-bit register address in C in an array referenced by the 8-bit
// register address in B 
case class APut(a:Short, b:Short, c:Short) extends Instruction
case class APutWide(a:Short, b:Short, c:Short) extends Instruction
case class APutObject(a:Short, b:Short, c:Short) extends Instruction
case class APutBoolean(a:Short, b:Short, c:Short) extends Instruction
case class APutByte(a:Short, b:Short, c:Short) extends Instruction
case class APutChar(a:Short, b:Short, c:Short) extends Instruction
case class APutShort(a:Short, b:Short, c:Short) extends Instruction

/* not really instructions, but added here to see if we can get a complete
 * reading of the instruction code finished */
case class PackedSwitchPayload(firstKey:Int, targets:Array[Int]) extends Instruction {
  override def toS(): String =
    "packed-switch-payload " + firstKey + ", {" + targets.mkString(", ")  + "}" 
}
case class SparseSwitchPayload(keys:Array[Int], targets:Array[Int]) extends Instruction {
  override def toS(): String =
    "sparse-switch-payload {" + keys.zip(targets).mkString(", ") + "}"
}
case class FillArrayDataPayload(size:Long, elementWidth:Int, data:Array[Short]) extends Instruction {
  override def toS(): String =
    "fill-array-data-payload " + size + ", " + elementWidth + ", {" +
       data.mkString(", ") + "}"
}
