package com.ucombinator.dalvik.AST

// http://www.milk.com/kodebase/dalvik-docs-mirror/docs/dalvik-bytecode.html

abstract class Instruction {
  var next: Instruction = null
  var sourceInfo: SourceInfo = null
}

case object End extends Instruction
case object Nop extends Instruction

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
case class Move(A:Byte, B:Byte) extends Instruction
case class MoveWide(A:Byte, B:Byte) extends Instruction
case class MoveObject(A:Byte, B:Byte) extends Instruction

// moves the result of an invoke instruction to a register (or register pair)
// with an 8-bit register address
case class MoveResult(A:Short) extends Instruction
case class MoveResultWide(A:Short) extends Instruction
case class MoveResultObject(A:Short) extends Instruction
case class MoveException(A:Short) extends Instruction

// moves from a register with a 16-bit register address to a register with an
// 8-bit register address
case class MoveFrom16(A:Short, B:Int) extends Instruction
case class MoveWideFrom16(A:Short, B:Int) extends Instruction
case class MoveObjectFrom16(A:Short, B:Int) extends Instruction

// moves from a register with a 16-bit register address to another register
// with a 16-bit register address
case class Move16(A:Int, B:Int) extends Instruction
case class MoveWide16(A:Int, B:Int) extends Instruction
case class MoveObject16(A:Int, B:Int) extends Instruction

// return a void value
case object ReturnVoid extends Instruction

// return a value from a register specified by a 16-bit register address
case class Return(A:Short) extends Instruction
case class ReturnWide(A:Short) extends Instruction
case class ReturnObject(A:Short) extends Instruction

// instructions for invoking methods

// Invoke the method indicated by B with the argument registers at the 4-bit
// register addresses referenced from the args array
case class InvokeSuper(args:Array[Byte], B:Method) extends Instruction
case class InvokeDirect(args:Array[Byte], B:Method) extends Instruction
case class InvokeStatic(args:Array[Byte], B:Method) extends Instruction
case class InvokeInterface(args:Array[Byte], B:Method) extends Instruction
case class InvokeVirtual(args:Array[Byte], B:Method) extends Instruction

// Invoke the method indicated by B with the argument registers starting at the
// 16-bit address referenced by C and until a total of A arguments are used
case class InvokeVirtualRange(C:Int, A:Short, B:Method) extends Instruction
case class InvokeSuperRange(C:Int, A:Short, B:Method) extends Instruction
case class InvokeDirectRange(C:Int, A:Short, B:Method) extends Instruction
case class InvokeStaticRange(C:Int, A:Short, B:Method) extends Instruction
case class InvokeInterfaceRange(C:Int, A:Short, B:Method) extends Instruction

// instructions for branching

// Jump, unconditionally, to the given target
case class Goto(A:Int) extends Instruction
case class Goto16(A:Int) extends Instruction
case class Goto32(A:Int) extends Instruction

// Jump to the relative target indicated in B when the value at the 8-bit
// register address in A is related to 0
case class IfEqz(A:Short, B:Int) extends Instruction
case class IfNez(A:Short, B:Int) extends Instruction
case class IfLtz(A:Short, B:Int) extends Instruction
case class IfGez(A:Short, B:Int) extends Instruction
case class IfGtz(A:Short, B:Int) extends Instruction
case class IfLez(A:Short, B:Int) extends Instruction

// Jump to the relative target indicated in C when the values at the 4-bit
// register address in A and B satisfy the given relationship
case class IfEq(A:Byte, B:Byte, C:Int) extends Instruction
case class IfNe(A:Byte, B:Byte, C:Int) extends Instruction
case class IfLt(A:Byte, B:Byte, C:Int) extends Instruction
case class IfGe(A:Byte, B:Byte, C:Int) extends Instruction
case class IfGt(A:Byte, B:Byte, C:Int) extends Instruction
case class IfLe(A:Byte, B:Byte, C:Int) extends Instruction

// Swithcing statements to support jump tables
// TODO: Need to supply the packed or spaces swithc payload to
// Jumps based on the value in the 8-bit register address referenced by A
case class TempPackedSwitch(A:Short, B:Int) extends Instruction
case class TempSparseSwitch(A:Short, B:Int) extends Instruction
case class PackedSwitch(A:Short, firstKey:Int, targets:Array[Int]) extends Instruction
case class SparseSwitch(A:Short, keys:Array[Int], targets:Array[Int]) extends Instruction

// throw the exception indicated in A
case class Throw(A:Short) extends Instruction

// instructions for entering and leaving monitors

// Enters or exits the the monitor indcated by the 8-bit register address in A
case class MonitorEnter(A:Short) extends Instruction
case class MonitorExit(A:Short) extends Instruction


// Comparison operators for floats, doubles, and long

// Compares the values from the 8-bit register addresses in B and C and writes
// the result to the 8-bit register address in A, where the result is
// 0 if [B] == [C], -1 if [B] < [C], or 1 if [B] > [C]
case class CmplFloat(A:Short, B:Short, C:Short) extends Instruction
case class CmpgFloat(A:Short, B:Short, C:Short) extends Instruction
case class CmplDouble(A:Short, B:Short, C:Short) extends Instruction
case class CmpgDouble(A:Short, B:Short, C:Short) extends Instruction
case class CmpLong(A:Short, B:Short, C:Short) extends Instruction


// instructions for loading constants

// stores the 32-bit (sign-extended if necessary) constant in B into a register
// with a 4-bit (Const4) or an 8-bit (Const16, Const) register address
case class Const4(A:Byte, B:Int) extends Instruction
case class Const16(A:Short, B:Int) extends Instruction
case class Const(A:Short, B:Int) extends Instruction

// stores the 32-bit (right-zero-extended) constant in B into a register with
// an 8-bit register address
case class ConstHigh16(A:Short, B:Int) extends Instruction

// stores the 64-bit (sign-extended if necessary) constant in B into a
// register-pair with an 8-bit register address
case class ConstWide16(A:Short, B:Long) extends Instruction
case class ConstWide32(A:Short, B:Long) extends Instruction
case class ConstWide(A:Short, B:Long) extends Instruction

// stores the 64-bit (right-zero-extended) constant in B into a register with
// an 8-bit register address
case class ConstWideHigh16(A:Short, B:Long) extends Instruction

// stores a String from the String constants table into a register with an
// 8-bit register address
case class ConstString(A:Short, B:String) extends Instruction
case class ConstStringJumbo(A:Short, B:String) extends Instruction

// stores the class (type) indicated by the associated into a register with an
// 8-bit register address
case class ConstClass(A:Short, var B:JavaType) extends Instruction

// Unary opertators

// Performs negation on the value in the register specified by the 4-bit
// register address in B and puts the value at the 4-bit register address in A
case class NegInt(A:Byte, B:Byte) extends Instruction
case class NegLong(A:Byte, B:Byte) extends Instruction
case class NegFloat(A:Byte, B:Byte) extends Instruction
case class NegDouble(A:Byte, B:Byte) extends Instruction

// Performs logical negation on the value in the register specified by the
// 4-bit register address in B and puts the value at the 4-bit register address
// in A
case class NotInt(A:Byte, B:Byte) extends Instruction
case class NotLong(A:Byte, B:Byte) extends Instruction

// Conversion operators

// convert value found at 4-bit register address in B into the appropriate type
// of value and store it in the 4-bit register address in A
case class IntToLong(A:Byte, B:Byte) extends Instruction
case class IntToFloat(A:Byte, B:Byte) extends Instruction
case class IntToDouble(A:Byte, B:Byte) extends Instruction
case class LongToInt(A:Byte, B:Byte) extends Instruction
case class LongToFloat(A:Byte, B:Byte) extends Instruction
case class LongToDouble(A:Byte, B:Byte) extends Instruction
case class FloatToInt(A:Byte, B:Byte) extends Instruction
case class FloatToLong(A:Byte, B:Byte) extends Instruction
case class FloatToDouble(A:Byte, B:Byte) extends Instruction
case class DoubleToInt(A:Byte, B:Byte) extends Instruction
case class DoubleToLong(A:Byte, B:Byte) extends Instruction
case class DoubleToFloat(A:Byte, B:Byte) extends Instruction
case class IntToByte(A:Byte, B:Byte) extends Instruction
case class IntToChar(A:Byte, B:Byte) extends Instruction
case class IntToShort(A:Byte, B:Byte) extends Instruction

// Arithmetic two address instructions

// perform arithmetic operation on values found a A and B 4-bit register
// address, and write result into A regster
case class AddInt2Addr(A:Byte, B:Byte) extends Instruction
case class SubInt2Addr(A:Byte, B:Byte) extends Instruction
case class MulInt2Addr(A:Byte, B:Byte) extends Instruction
case class DivInt2Addr(A:Byte, B:Byte) extends Instruction
case class RemInt2Addr(A:Byte, B:Byte) extends Instruction
case class AndInt2Addr(A:Byte, B:Byte) extends Instruction
case class OrInt2Addr(A:Byte, B:Byte) extends Instruction
case class XorInt2Addr(A:Byte, B:Byte) extends Instruction
case class ShlInt2Addr(A:Byte, B:Byte) extends Instruction
case class ShrInt2Addr(A:Byte, B:Byte) extends Instruction
case class UshrInt2Addr(A:Byte, B:Byte) extends Instruction
case class AddLong2Addr(A:Byte, B:Byte) extends Instruction
case class SubLong2Addr(A:Byte, B:Byte) extends Instruction
case class MulLong2Addr(A:Byte, B:Byte) extends Instruction
case class DivLong2Addr(A:Byte, B:Byte) extends Instruction
case class RemLong2Addr(A:Byte, B:Byte) extends Instruction
case class AndLong2Addr(A:Byte, B:Byte) extends Instruction
case class OrLong2Addr(A:Byte, B:Byte) extends Instruction
case class XorLong2Addr(A:Byte, B:Byte) extends Instruction
case class ShlLong2Addr(A:Byte, B:Byte) extends Instruction
case class ShrLong2Addr(A:Byte, B:Byte) extends Instruction
case class UshrLong2Addr(A:Byte, B:Byte) extends Instruction
case class AddFloat2Addr(A:Byte, B:Byte) extends Instruction
case class SubFloat2Addr(A:Byte, B:Byte) extends Instruction
case class MulFloat2Addr(A:Byte, B:Byte) extends Instruction
case class DivFloat2Addr(A:Byte, B:Byte) extends Instruction
case class RemFloat2Addr(A:Byte, B:Byte) extends Instruction
case class AddDouble2Addr(A:Byte, B:Byte) extends Instruction
case class SubDouble2Addr(A:Byte, B:Byte) extends Instruction
case class MulDouble2Addr(A:Byte, B:Byte) extends Instruction
case class DivDouble2Addr(A:Byte, B:Byte) extends Instruction
case class RemDouble2Addr(A:Byte, B:Byte) extends Instruction

// Arithmetic three address instructions

// perform arithmetic opertion on values found at B and C 8-bit register
// addresses and write result into A
case class AddInt(A:Short, B:Short, C:Short) extends Instruction
case class SubInt(A:Short, B:Short, C:Short) extends Instruction
case class MulInt(A:Short, B:Short, C:Short) extends Instruction
case class DivInt(A:Short, B:Short, C:Short) extends Instruction
case class RemInt(A:Short, B:Short, C:Short) extends Instruction
case class AndInt(A:Short, B:Short, C:Short) extends Instruction
case class OrInt(A:Short, B:Short, C:Short) extends Instruction
case class XorInt(A:Short, B:Short, C:Short) extends Instruction
case class ShlInt(A:Short, B:Short, C:Short) extends Instruction
case class ShrInt(A:Short, B:Short, C:Short) extends Instruction
case class UshrInt(A:Short, B:Short, C:Short) extends Instruction
case class AddLong(A:Short, B:Short, C:Short) extends Instruction
case class SubLong(A:Short, B:Short, C:Short) extends Instruction
case class MulLong(A:Short, B:Short, C:Short) extends Instruction
case class DivLong(A:Short, B:Short, C:Short) extends Instruction
case class RemLong(A:Short, B:Short, C:Short) extends Instruction
case class AndLong(A:Short, B:Short, C:Short) extends Instruction
case class OrLong(A:Short, B:Short, C:Short) extends Instruction
case class XorLong(A:Short, B:Short, C:Short) extends Instruction
case class ShlLong(A:Short, B:Short, C:Short) extends Instruction
case class ShrLong(A:Short, B:Short, C:Short) extends Instruction
case class UshrLong(A:Short, B:Short, C:Short) extends Instruction
case class AddFloat(A:Short, B:Short, C:Short) extends Instruction
case class SubFloat(A:Short, B:Short, C:Short) extends Instruction
case class MulFloat(A:Short, B:Short, C:Short) extends Instruction
case class DivFloat(A:Short, B:Short, C:Short) extends Instruction
case class RemFloat(A:Short, B:Short, C:Short) extends Instruction
case class AddDouble(A:Short, B:Short, C:Short) extends Instruction
case class SubDouble(A:Short, B:Short, C:Short) extends Instruction
case class MulDouble(A:Short, B:Short, C:Short) extends Instruction
case class DivDouble(A:Short, B:Short, C:Short) extends Instruction
case class RemDouble(A:Short, B:Short, C:Short) extends Instruction

// Arithmetic with 32-bit (sign extended from 8-bit) literal integer

// perform arithmetic operation on value found at B 8-bit register and inline
// literal and put result in A
case class AddIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class RsubIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class MulIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class DivIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class RemIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class AndIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class OrIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class XorIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class ShlIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class ShrIntLit8(A:Short, B:Short, C:Int) extends Instruction
case class UshrIntLit8(A:Short, B:Short, C:Int) extends Instruction

// Arithmetic with 32-bit (sign extended from 16-bit) literal integer

// perform arithmetic operation on value found at B 4-bit register and the
// inline literal and put result in A
case class AddIntLit16(A:Byte, B:Byte, C:Int) extends Instruction
case class RsubInt(A:Byte, B:Byte, C:Int) extends Instruction
case class MulIntLit16(A:Byte, B:Byte, C:Int) extends Instruction
case class DivIntLit16(A:Byte, B:Byte, C:Int) extends Instruction
case class RemIntLit16(A:Byte, B:Byte, C:Int) extends Instruction
case class AndIntLit16(A:Byte, B:Byte, C:Int) extends Instruction
case class OrIntLit16(A:Byte, B:Byte, C:Int) extends Instruction
case class XorIntLit16(A:Byte, B:Byte, C:Int) extends Instruction

// Instructions for getting and setting instance variables on objects

// Check that the value referenced from the 8-bit register address in A can be
// cast to the type in B
case class CheckCast(A:Short, var B:JavaType) extends Instruction

// Create a new instance of the class indicated in B, and put the reslt in the
// 8-bit register address in A
case class NewInstance(A:Short, var B:JavaType) extends Instruction

// Determines if the object referenced from the 4-bit register address in B is
// of the class type referenced in C and puts the result in A, where the result
// is 1 when [B] is an instance of [C] and 0 in any other case
case class InstanceOf(A:Byte, B:Byte, var C:JavaType) extends Instruction

// Retrieves a value from the field specified in C of the object referenced
// from the 4-bit register address in B and puts the result in the register or
// register-pair addressed by the 4-bit address in A.
case class IGet(A:Byte, B:Byte, C:Field) extends Instruction
case class IGetWide(A:Byte, B:Byte, C:Field) extends Instruction
case class IGetObject(A:Byte, B:Byte, C:Field) extends Instruction
case class IGetBoolean(A:Byte, B:Byte, C:Field) extends Instruction
case class IGetByte(A:Byte, B:Byte, C:Field) extends Instruction
case class IGetChar(A:Byte, B:Byte, C:Field) extends Instruction
case class IGetShort(A:Byte, B:Byte, C:Field) extends Instruction

// Puts the value reverenced from the 4-bit register address in A into the the
// field specified in C of the object referenced from the 4-bit register
// address in B
case class IPut(A:Byte, B:Byte, C:Field) extends Instruction
case class IPutWide(A:Byte, B:Byte, C:Field) extends Instruction
case class IPutObject(A:Byte, B:Byte, C:Field) extends Instruction
case class IPutBoolean(A:Byte, B:Byte, C:Field) extends Instruction
case class IPutByte(A:Byte, B:Byte, C:Field) extends Instruction
case class IPutChar(A:Byte, B:Byte, C:Field) extends Instruction
case class IPutShort(A:Byte, B:Byte, C:Field) extends Instruction

// Instructions for getting and setting static variables on classes

// Retrieves a value from the class field specified in B and puts it in the
// register or register-pair referenced by the 8-bit register address in A
case class SGet(A:Short, B:Field) extends Instruction
case class SGetWide(A:Short, B:Field) extends Instruction
case class SGetObject(A:Short, B:Field) extends Instruction
case class SGetBoolean(A:Short, B:Field) extends Instruction
case class SGetByte(A:Short, B:Field) extends Instruction
case class SGetChar(A:Short, B:Field) extends Instruction
case class SGetShort(A:Short, B:Field) extends Instruction

// Puts a value from the register or register-pair referenced by the 8-bit
// register address in A into the class field specified in B 
case class SPut(A:Short, B:Field) extends Instruction
case class SPutWide(A:Short, B:Field) extends Instruction
case class SPutObject(A:Short, B:Field) extends Instruction
case class SPutBoolean(A:Short, B:Field) extends Instruction
case class SPutByte(A:Short, B:Field) extends Instruction
case class SPutChar(A:Short, B:Field) extends Instruction
case class SPutShort(A:Short, B:Field) extends Instruction

// Instructions for interacting with arrays

// Determines the length of the array referenced from the 4-bit register
// address in B and put the result at the 4-bit register address in A
case class ArrayLength(A:Byte, B:Byte) extends Instruction

// Creates a new Array of type C of the size stored in the 4-bit register in B
// and put the result in the 4-bit register refrenced from A
case class NewArray(A:Byte, B:Byte, var C:JavaType)  extends Instruction

// TODO: need to supply the FillArrayDataPayload, indicated in the B
// Stores the array values found in a fill-array-data-payload into the array
// referenced from the 8-bit register address in A
case class TempFillArrayData(A:Short, B:Int) extends Instruction
case class FillArrayData(A:Short, size:Long, elementWidth:Int, data:Array[Short]) extends Instruction

// Creates a new filled array of the type in B with the values from the 4-bit
// register addresses referenced by the args and places the result in the
// same place as the result of calling an invoke instruction;  the result
// can be moved using an immediately subsequent move-result-object instruction
case class FilledNewArray(args:Array[Byte], var B:JavaType) extends Instruction

// Creates a new filled array of the type in B with the values from the set
// of registers starting at the 16-bit register address indicated by C for the
// count stored in A and places the result in the same place as the result of
// calling an invoke instruction;  the result can be moved using an immediately
// subsequent move-result-object instruction
case class FilledNewArrayRange(C:Int, A:Short, var B:JavaType) extends Instruction


// Retrieves a value from the index referenced in the 8-bit register
// address in C in an array referenced by the 8-bit register address in B and
// puts it into the 8-bit register address in A
case class AGet(A:Short, B:Short, C:Short) extends Instruction
case class AGetWide(A:Short, B:Short, C:Short) extends Instruction
case class AGetObject(A:Short, B:Short, C:Short) extends Instruction
case class AGetBoolean(A:Short, B:Short, C:Short) extends Instruction
case class AGetByte(A:Short, B:Short, C:Short) extends Instruction
case class AGetChar(A:Short, B:Short, C:Short) extends Instruction
case class AGetShort(A:Short, B:Short, C:Short) extends Instruction

// Puts a value from the 8-bit register address in A into the index referenced
// in the 8-bit register address in C in an array referenced by the 8-bit
// register address in B 
case class APut(A:Short, B:Short, C:Short) extends Instruction
case class APutWide(A:Short, B:Short, C:Short) extends Instruction
case class APutObject(A:Short, B:Short, C:Short) extends Instruction
case class APutBoolean(A:Short, B:Short, C:Short) extends Instruction
case class APutByte(A:Short, B:Short, C:Short) extends Instruction
case class APutChar(A:Short, B:Short, C:Short) extends Instruction
case class APutShort(A:Short, B:Short, C:Short) extends Instruction

/* not really instructions, but added here to see if we can get a complete
 * reading of the instruction code finished */
case class PackedSwitchPayload(firstKey:Int, targets:Array[Int]) extends Instruction
case class SparseSwitchPayload(keys:Array[Int], targets:Array[Int]) extends Instruction
case class FillArrayDataPayload(size:Long, elementWidth:Int, data:Array[Short]) extends Instruction
