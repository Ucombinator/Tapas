package com.ucombinator.dalvik

import java.io.{PrintStream, FileOutputStream}
import com.ucombinator.dalvik.android.ApkReader
import com.ucombinator.dalvik.AST._
import com.ucombinator.dalvik.analysis.{SimpleMethodCallGraph, SourceSinkMethodCallAnalyzer, IntraproceduralAnalyzer}

object Analyzer extends App {
  var apkFile: String = null
  var dump = false
  var outputFile: String = null
  var databaseFile: String = null
  var configFile: String = "config/sourceSink.xml" // set our default file location
  var className: String = null
  var methodName: String = null
  var intra: Boolean = false

  private def displayHelpMessage = {
    println("usage: analyzer [<options>] APK-file")
    println("  -h | --help        :: print this message")
    println("  -d | --dump        :: dump out the class definitions")
    println("  -o | --output-file :: set the file for dump")
    println("  -c | --class-name  :: indicate the class name to analyze")
    println("  -m | --method-name :: indicate the method name to analyze")
    println("  -f | --config      :: set the configuration file")
    sys.exit
  }

  private def parseOptions(args:List[String]):Unit = {
    args match {
      case ("-h"  | "--help") :: rest => displayHelpMessage
      case ("-d"  | "--dump") :: rest => dump = true ; parseOptions(rest)
      case ("-i"  | "--intraprocedural") :: rest => intra = true ; parseOptions(rest)
      case ("-o"  | "--output-file") :: fn :: rest => outputFile = fn ; parseOptions(rest)
      case ("-db" | "--database") :: fn :: rest => databaseFile = fn ; parseOptions(rest)
      case ("-c"  | "--class-name") :: cn :: rest => className = cn ; parseOptions(rest)
      case ("-f"  | "--config") :: fn :: rest => configFile = fn ; parseOptions(rest)
      case ("-m"  | "--method-name") :: mn :: rest => methodName = mn ; parseOptions(rest)
      case fn :: rest => apkFile = fn ; parseOptions(rest)
      case Nil => Unit
      case _ => println("unrecognized option: " + args) ; displayHelpMessage
    }
  }

  private def javaTypeToName(t: JavaType): String = {
    t match {
      case VoidType => "void"
      case BooleanType => "boolean"
      case ByteType => "byte"
      case ShortType => "short"
      case CharType => "char"
      case IntType => "int"
      case LongType => "long"
      case FloatType => "float"
      case DoubleType => "double"
      case at: AbstractType => canonicalClassName(at.nameOf)
      case ar: ArrayType => "Array[" + javaTypeToName(ar.typeOf) + "]"
      case cd: ClassDef => canonicalClassName(cd.name)
      case _ => throw new Exception("Unrecognized JavaType: " + t)
    }
  }

  private def javaTypeToClassName(t: JavaType): String = {
    t match {
      case cd: ClassDef => cd.name
      case at: AbstractType => at.nameOf
      case _ => throw new Exception("JavaType does not have classname: " + t)
    }
  }

  private def canonicalClassName(s: String): String = 
    s.substring(1, s.length - 1).replace("/", ".")

  private def javaTypeToCanonicalClassName(t: JavaType): String =
    canonicalClassName(javaTypeToClassName(t))

  private def printFields(fieldType: String, fields: Array[FieldDef]): Unit = {
    if (fields != null && fields.length > 0) {
      println("  " + fieldType + " fields:")
      for(f <- fields) {
        val fld = f.field
        println("    " + javaTypeToName(fld.fieldType) + " " + fld.name)
      }
    }
  }

  private def printMethods(methodType: String, methods: Array[MethodDef]): Unit = {
    if (methods != null && methods.length > 0) {
      println("  " + methodType + " methods:")
      for(m <- methods) {
        val meth = m.method
        val proto = meth.prototype
        println("    " + javaTypeToName(proto.returnType) + " " + meth.name + "(" +
          (if (proto.parameters == null)
             ""
           else
             (proto.parameters map javaTypeToName).mkString(", ")) + ")")
        printCodeItem(m.code)
      }
    }
  }

  private def printCodeItem(ci: CodeItem): Unit = {
    if (ci != null) {
      println("      register count: " + ci.registersSize)
      println("      ins count: " + ci.insSize)
      println("      outs count: " + ci.outsSize)
      for(idx <- 0 until ci.insns.length) {
        println("      " + idx + ":\t" + instructionToString(ci.insns(idx)))
      }
    }
  }

  private def instructionToString(i: Instruction): String = {
    i match {
      case End => "end"
      case Nop => "nop"
      case Move(a, b) => "move v" + a + ", v" + b
      case MoveWide(a, b) => "move-wide v" + a + ", v" + b
      case MoveObject(a, b) => "move-object v" + a + ", v" + b
      case MoveResult(a) => "move-result v" + a
      case MoveResultWide(a) => "move-result-wide v" + a
      case MoveResultObject(a) => "move-result-object v" + a
      case MoveException(a) => "move-exception v" + a
      case MoveFrom16(a, b) => "move/from16 v" + a + ", v" + b
      case MoveWideFrom16(a, b) => "move-wide/from16 v" + a + ", v" + b
      case MoveObjectFrom16(a, b) => "move-object/from16 v" + a + ", v" + b
      case Move16(a, b) => "move/16 v" + a + ", v" + b
      case MoveWide16(a, b) => "move-wide/16 v" + a + ", v" + b
      case MoveObject16(a, b) => "move-object/16 v" + a + ", v" + b
      case ReturnVoid => "return-void"
      case Return(a) => "return v" + a
      case ReturnWide(a) => "return-wide v" + a
      case ReturnObject(a) => "return-object v" + a
      case InvokeSuper(args, b) => "invoke-super {v" + args.mkString(", v") + "} " + javaTypeToName(b.classType) + "." + b.name
      case InvokeDirect(args, b) => "invoke-direct {v" + args.mkString(", v") + "} " + javaTypeToName(b.classType) + "." + b.name
      case InvokeStatic(args, b) => "invoke-static {v" + args.mkString(", v") + "} " + javaTypeToName(b.classType) + "." + b.name
      case InvokeInterface(args, b) => "invoke-interface {v" + args.mkString(", v") + "} " + javaTypeToName(b.classType) + "." + b.name
      case InvokeVirtual(args, b) => "invoke-virtual {v" + args.mkString(", v") + "} " + javaTypeToName(b.classType) + "." + b.name
      case InvokeVirtualRange(c, a, b) => "invoke-virtual/range " + rangeToVarRef(a, c) + " " + javaTypeToName(b.classType) + "." + b.name
      case InvokeSuperRange(c, a, b) => "invoke-super/range " + rangeToVarRef(a, c) + " " + javaTypeToName(b.classType) + "." + b.name
      case InvokeDirectRange(c, a, b) => "invoke-direct/range " + rangeToVarRef(a, c) + " " + javaTypeToName(b.classType) + "." + b.name
      case InvokeStaticRange(c, a, b) => "invoke-static/range " + rangeToVarRef(a, c) + " " + javaTypeToName(b.classType) + "." + b.name
      case InvokeInterfaceRange(c, a, b) => "invoke-interface/range " + rangeToVarRef(a, c) + " " + javaTypeToName(b.classType) + "." + b.name
      case Goto(a) => "goto " + a
      case Goto16(a) => "goto/16 " + a
      case Goto32(a) => "goto/32 " + a
      case IfEqz(a, b) => "if-eqz v" + a + ", " + b
      case IfNez(a, b) => "if-nez v" + a + ", " + b
      case IfLtz(a, b) => "if-ltz v" + a + ", " + b
      case IfGez(a, b) => "if-gez v" + a + ", " + b
      case IfGtz(a, b) => "if-gtz v" + a + ", " + b
      case IfLez(a, b) => "if-lez v" + a + ", " + b
      case IfEq(a, b, c) => "if-eq v" + a + ", v" + b + ", " + c
      case IfNe(a, b, c) => "if-ne v" + a + ", v" + b + ", " + c
      case IfLt(a, b, c) => "if-lt v" + a + ", v" + b + ", " + c
      case IfGe(a, b, c) => "if-ge v" + a + ", v" + b + ", " + c
      case IfGt(a, b, c) => "if-gt v" + a + ", v" + b + ", " + c
      case IfLe(a, b, c) => "if-le v" + a + ", v" + b + ", " + c
      case PackedSwitch(a, firstKey, targets) => "packed-switch v" + a + ", " + firstKey + ", {" + targets.mkString(", ")  + "}"
      case TempPackedSwitch(a, b) => "packed-switch v" + a + ", " + b
      case SparseSwitch(a, keys, targets) => "sparse-switch v" + a + " {" + keys.zip(targets).mkString(", ") + "}"
      case TempSparseSwitch(a, b) => "sparse-switch v" + a + ", " + b
      case Throw(a) => "throw v" + a
      case MonitorEnter(a) => "monitor-enter v" + a
      case MonitorExit(a) => "monitor-exit v" + a
      case CmplFloat(a, b, c) => "Cmpl-float v" + a + ", v" + b + ", v" + c
      case CmpgFloat(a, b, c) => "cmpg-float v" + a + ", v" + b + ", v" + c
      case CmplDouble(a, b, c) => "Cmpl-double v" + a + ", v" + b + ", v" + c
      case CmpgDouble(a, b, c) => "cmpg-double v" + a + ", v" + b + ", v" + c
      case CmpLong(a, b, c) => "cmp-long v" + a + ", v" + b + ", v" + c
      case Const4(a, b) => "const/4 v" + a + ", " + b
      case Const16(a, b) => "const/16 v" + a + ", " + b
      case Const(a, b) => "const v" + a + ", " + b
      case ConstHigh16(a, b) => "const/high16 v" + a + ", " + b
      case ConstWide16(a, b) => "const-wide/16 v" + a + ", " + b
      case ConstWide32(a, b) => "const-wide/32 v" + a + ", " + b
      case ConstWide(a, b) => "const-wide v" + a + ", " + b
      case ConstWideHigh16(a, b) => "const-wide/high16 v" + a + ", " + b
      case ConstString(a, b) => "const-string v" + a + ", \"" + b + "\""
      case ConstStringJumbo(a, b) => "const-string/jumbo v" + a + ", \"" + b + "\""
      case ConstClass(a, b) => "const-class v" + a + ", " + javaTypeToName(b)
      case NegInt(a, b) => "neg-int v" + a + ", v" + b
      case NegLong(a, b) => "neg-long v" + a + ", v" + b
      case NegFloat(a, b) => "neg-float v" + a + ", v" + b
      case NegDouble(a, b) => "neg-double v" + a + ", v" + b
      case NotInt(a, b) => "not-int v" + a + ", v" + b
      case NotLong(a, b) => "not-long v" + a + ", v" + b
      case IntToLong(a, b) => "int-to-long v" + a + ", v" + b
      case IntToFloat(a, b) => "int-to-float v" + a + ", v" + b
      case IntToDouble(a, b) => "int-to-double v" + a + ", v" + b
      case LongToInt(a, b) => "long-to-int v" + a + ", v" + b
      case LongToFloat(a, b) => "long-to-float v" + a + ", v" + b
      case LongToDouble(a, b) => "long-to-double v" + a + ", v" + b
      case FloatToInt(a, b) => "float-to-int v" + a + ", v" + b
      case FloatToLong(a, b) => "float-to-long v" + a + ", v" + b
      case FloatToDouble(a, b) => "float-to-double v" + a + ", v" + b
      case DoubleToInt(a, b) => "double-to-int v" + a + ", v" + b
      case DoubleToLong(a, b) => "double-to-long v" + a + ", v" + b
      case DoubleToFloat(a, b) => "double-to-float v" + a + ", v" + b
      case IntToByte(a, b) => "int-to-byte v" + a + ", v" + b
      case IntToChar(a, b) => "int-to-char v" + a + ", v" + b
      case IntToShort(a, b) => "int-to-short v" + a + ", v" + b
      case AddInt2Addr(a, b) => "add-int/2addr v" + a + ", v" + b
      case SubInt2Addr(a, b) => "sub-int/2addr v" + a + ", v" + b
      case MulInt2Addr(a, b) => "mul-int/2addr v" + a + ", v" + b
      case DivInt2Addr(a, b) => "div-int/2addr v" + a + ", v" + b
      case RemInt2Addr(a, b) => "rem-int/2addr v" + a + ", v" + b
      case AndInt2Addr(a, b) => "and-int/2addr v" + a + ", v" + b
      case OrInt2Addr(a, b) => "or-int/2addr v" + a + ", v" + b
      case XorInt2Addr(a, b) => "xor-int/2addr v" + a + ", v" + b
      case ShlInt2Addr(a, b) => "shl-int/2addr v" + a + ", v" + b
      case ShrInt2Addr(a, b) => "shr-int/2addr v" + a + ", v" + b
      case UshrInt2Addr(a, b) => "ushr-int/2addr v" + a + ", v" + b
      case AddLong2Addr(a, b) => "add-long/2addr v" + a + ", v" + b
      case SubLong2Addr(a, b) => "sub-long/2addr v" + a + ", v" + b
      case MulLong2Addr(a, b) => "mul-long/2addr v" + a + ", v" + b
      case DivLong2Addr(a, b) => "div-long/2addr v" + a + ", v" + b
      case RemLong2Addr(a, b) => "rem-long/2addr v" + a + ", v" + b
      case AndLong2Addr(a, b) => "and-long/2addr v" + a + ", v" + b
      case OrLong2Addr(a, b) => "or-long/2addr v" + a + ", v" + b
      case XorLong2Addr(a, b) => "xor-long/2addr v" + a + ", v" + b
      case ShlLong2Addr(a, b) => "shl-long/2addr v" + a + ", v" + b
      case ShrLong2Addr(a, b) => "shr-long/2addr v" + a + ", v" + b
      case UshrLong2Addr(a, b) => "ushr-long/2addr v" + a + ", v" + b
      case AddFloat2Addr(a, b) => "add-float/2addr v" + a + ", v" + b
      case SubFloat2Addr(a, b) => "sub-float/2addr v" + a + ", v" + b
      case MulFloat2Addr(a, b) => "mul-float/2addr v" + a + ", v" + b
      case DivFloat2Addr(a, b) => "div-float/2addr v" + a + ", v" + b
      case RemFloat2Addr(a, b) => "rem-float/2addr v" + a + ", v" + b
      case AddDouble2Addr(a, b) => "add-double/2addr v" + a + ", v" + b
      case SubDouble2Addr(a, b) => "sub-double/2addr v" + a + ", v" + b
      case MulDouble2Addr(a, b) => "mul-double/2addr v" + a + ", v" + b
      case DivDouble2Addr(a, b) => "div-double/2addr v" + a + ", v" + b
      case RemDouble2Addr(a, b) => "rem-double/2addr v" + a + ", v" + b
      case AddInt(a, b, c) => "add-int v" + a + ", v" + b + ", v" + c
      case SubInt(a, b, c) => "sub-int v" + a + ", v" + b + ", v" + c
      case MulInt(a, b, c) => "mul-int v" + a + ", v" + b + ", v" + c
      case DivInt(a, b, c) => "div-int v" + a + ", v" + b + ", v" + c
      case RemInt(a, b, c) => "rem-int v" + a + ", v" + b + ", v" + c
      case AndInt(a, b, c) => "and-int v" + a + ", v" + b + ", v" + c
      case OrInt(a, b, c) => "or-int v" + a + ", v" + b + ", v" + c
      case XorInt(a, b, c) => "xor-int v" + a + ", v" + b + ", v" + c
      case ShlInt(a, b, c) => "shl-int v" + a + ", v" + b + ", v" + c
      case ShrInt(a, b, c) => "shr-int v" + a + ", v" + b + ", v" + c
      case UshrInt(a, b, c) => "ushr-int v" + a + ", v" + b + ", v" + c
      case AddLong(a, b, c) => "add-long v" + a + ", v" + b + ", v" + c
      case SubLong(a, b, c) => "sub-long v" + a + ", v" + b + ", v" + c
      case MulLong(a, b, c) => "mul-long v" + a + ", v" + b + ", v" + c
      case DivLong(a, b, c) => "div-long v" + a + ", v" + b + ", v" + c
      case RemLong(a, b, c) => "rem-long v" + a + ", v" + b + ", v" + c
      case AndLong(a, b, c) => "and-long v" + a + ", v" + b + ", v" + c
      case OrLong(a, b, c) => "or-long v" + a + ", v" + b + ", v" + c
      case XorLong(a, b, c) => "xor-long v" + a + ", v" + b + ", v" + c
      case ShlLong(a, b, c) => "shl-long v" + a + ", v" + b + ", v" + c
      case ShrLong(a, b, c) => "shr-long v" + a + ", v" + b + ", v" + c
      case UshrLong(a, b, c) => "ushr-long v" + a + ", v" + b + ", v" + c
      case AddFloat(a, b, c) => "add-float v" + a + ", v" + b + ", v" + c
      case SubFloat(a, b, c) => "sub-float v" + a + ", v" + b + ", v" + c
      case MulFloat(a, b, c) => "mul-float v" + a + ", v" + b + ", v" + c
      case DivFloat(a, b, c) => "div-float v" + a + ", v" + b + ", v" + c
      case RemFloat(a, b, c) => "rem-float v" + a + ", v" + b + ", v" + c
      case AddDouble(a, b, c) => "add-double v" + a + ", v" + b + ", v" + c
      case SubDouble(a, b, c) => "sub-double v" + a + ", v" + b + ", v" + c
      case MulDouble(a, b, c) => "mul-double v" + a + ", v" + b + ", v" + c
      case DivDouble(a, b, c) => "div-double v" + a + ", v" + b + ", v" + c
      case RemDouble(a, b, c) => "rem-double v" + a + ", v" + b + ", v" + c
      case AddIntLit8(a, b, c) => "add-int/lit8 v" + a + ", v" + b + ", " + c
      case RsubIntLit8(a, b, c) => "rsub-int/lit8 v" + a + ", v" + b + ", " + c
      case MulIntLit8(a, b, c) => "mul-int/lit8 v" + a + ", v" + b + ", " + c
      case DivIntLit8(a, b, c) => "div-int/lit8 v" + a + ", v" + b + ", " + c
      case RemIntLit8(a, b, c) => "rem-int/lit8 v" + a + ", v" + b + ", " + c
      case AndIntLit8(a, b, c) => "and-int/lit8 v" + a + ", v" + b + ", " + c
      case OrIntLit8(a, b, c) => "or-int/lit8 v" + a + ", v" + b + ", " + c
      case XorIntLit8(a, b, c) => "xor-int/lit8 v" + a + ", v" + b + ", " + c
      case ShlIntLit8(a, b, c) => "shl-int/lit8 v" + a + ", v" + b + ", " + c
      case ShrIntLit8(a, b, c) => "shr-int/lit8 v" + a + ", v" + b + ", " + c
      case UshrIntLit8(a, b, c) => "ushr-int/lit8 v" + a + ", v" + b + ", " + c
      case AddIntLit16(a, b, c) => "add-int/lit16 v" + a + ", v" + b + ", " + c
      case RsubInt(a, b, c) => "rsub-int v" + a + ", v" + b + ", " + c
      case MulIntLit16(a, b, c) => "mul-int/lit16 v" + a + ", v" + b + ", " + c
      case DivIntLit16(a, b, c) => "div-int/lit16 v" + a + ", v" + b + ", " + c
      case RemIntLit16(a, b, c) => "rem-int/lit16 v" + a + ", v" + b + ", " + c
      case AndIntLit16(a, b, c) => "and-int/lit16 v" + a + ", v" + b + ", " + c
      case OrIntLit16(a, b, c) => "or-int/lit16 v" + a + ", v" + b + ", " + c
      case XorIntLit16(a, b, c) => "xor-int/lit16 v" + a + ", v" + b + ", " + c
      case CheckCast(a, b) => "check-cast v" + a + ", " + javaTypeToName(b)
      case NewInstance(a, b) => "new-instance v" + a + ", " + javaTypeToName(b)
      case InstanceOf(a, b, c) => "instance-of v" + a +", v" + b + ", " + javaTypeToName(c)
      case IGet(a, b, c) => "iget v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IGetWide(a, b, c) => "iget-wide v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IGetObject(a, b, c) => "iget-object v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IGetBoolean(a, b, c) => "iget-boolean v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IGetByte(a, b, c) => "iget-byte v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IGetChar(a, b, c) => "iget-char v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IGetShort(a, b, c) => "iget-short v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IPut(a, b, c) => "iput v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IPutWide(a, b, c) => "iput-wide v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IPutObject(a, b, c) => "iput-object v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IPutBoolean(a, b, c) => "iput-boolean v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IPutByte(a, b, c) => "iput-byte v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IPutChar(a, b, c) => "iput-char v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case IPutShort(a, b, c) => "iput-short v" + a + ", v" + b + ", " + javaTypeToName(c.classType) + "." + c.name
      case SGet(a, b) => "sget v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SGetWide(a, b) => "sget-wide v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SGetObject(a, b) => "sget-object v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SGetBoolean(a, b) => "sget-boolean v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SGetByte(a, b) => "sget-byte v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SGetChar(a, b) => "sget-char v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SGetShort(a, b) => "sget-short v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SPut(a, b) => "sput v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SPutWide(a, b) => "sput-wide v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SPutObject(a, b) => "sput-object v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SPutBoolean(a, b) => "sput-boolean v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SPutByte(a, b) => "sput-byte v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SPutChar(a, b) => "sput-char v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case SPutShort(a, b) => "sput-short v" + a + ", " + javaTypeToName(b.classType) + "." + b.name
      case ArrayLength(a, b) => "array-length v" + a + ", v" + b
      case NewArray(a, b, c)  => "new-array v" + a + ", v" + b + ", " + javaTypeToName(c)
      case FillArrayData(a, size, elementWidth, data) => "fill-array-data v" + a + ", " + size + ", " + elementWidth + ", {" + data.mkString(", ") + "}"
      case TempFillArrayData(a, b) => "fill-array-data v" + a + ", " + b
      case FilledNewArray(args, b) => "filled-new-array {v" + args.mkString(", v") + "} " + javaTypeToName(b)
      case FilledNewArrayRange(c, a, b) => "filled-new-array/range {v" + rangeToVarRef(a, c) + "} " + javaTypeToName(b)
      case AGet(a, b, c) => "aget v" + a + ", v" + b + ", v" + c
      case AGetWide(a, b, c) => "aget-wide v" + a + ", v" + b + ", v" + c
      case AGetObject(a, b, c) => "aget-object v" + a + ", v" + b + ", v" + c
      case AGetBoolean(a, b, c) => "aget-boolean v" + a + ", v" + b + ", v" + c
      case AGetByte(a, b, c) => "aget-byte v" + a + ", v" + b + ", v" + c
      case AGetChar(a, b, c) => "aget-char v" + a + ", v" + b + ", v" + c
      case AGetShort(a, b, c) => "aget-short v" + a + ", v" + b + ", v" + c
      case APut(a, b, c) => "aput v" + a + ", v" + b + ", v" + c
      case APutWide(a, b, c) => "aput-wide v" + a + ", v" + b + ", v" + c
      case APutObject(a, b, c) => "aput-object v" + a + ", v" + b + ", v" + c
      case APutBoolean(a, b, c) => "aput-boolean v" + a + ", v" + b + ", v" + c
      case APutByte(a, b, c) => "aput-byte v" + a + ", v" + b + ", v" + c
      case APutChar(a, b, c) => "aput-char v" + a + ", v" + b + ", v" + c
      case APutShort(a, b, c) => "aput-short v" + a + ", v" + b + ", v" + c
      case PackedSwitchPayload(firstKey, targets) => "packed-switch-payload " + firstKey + ", {" + targets.mkString(", ")  + "}"
      case SparseSwitchPayload(keys, targets) => "sparse-switch-payload {" + keys.zip(targets).mkString(", ") + "}"
      case FillArrayDataPayload(size, elementWidth, data) => "fill-array-data-payload " + size + ", " + elementWidth + ", {" + data.mkString(", ") + "}"
      case _ => throw new Exception("unrecognized instruction: " + i)
    }
  }

  private def rangeToVarRef(A:Short, C:Int):String = {
    val N = A + C - 1
    if (C == N) "{v" + C + "}" else "{v" + C + " .. v" + N + "}"
  }

  private def dumpClassDefs(classDefs: Array[ClassDef]): Unit = {
    for (cd <- classDefs) {
      print("class: " + canonicalClassName(cd.name))
      if (cd.superClass != null) {
        val superName = javaTypeToClassName(cd.superClass)
        if (superName != "Ljava/lang/Object;")
          print(" (super: " + canonicalClassName(superName) + ")")
      }
      println
      if (cd.interfaces != null)
        println("  interfaces: " + (cd.interfaces map javaTypeToCanonicalClassName).mkString(", "))
      printFields("static", cd.staticFields)
      printFields("instance", cd.instanceFields)
      printMethods("virtual", cd.virtualMethods)
      printMethods("direct", cd.directMethods)
    }
  }

  private def wrapOutput[T](thunk: => T) : T = {
    if (outputFile == null) thunk
    else Console.withOut(new PrintStream(new FileOutputStream(outputFile, true))) { thunk }
  }

  parseOptions(args.toList)

  if (apkFile == null) displayHelpMessage

  val apkReader = new ApkReader(apkFile)
  val classDefs = apkReader.readFile
  val simpleCallGraph = new SimpleMethodCallGraph(classDefs)
  val intraproceduralAnalyzer = if (intra) {
    new IntraproceduralAnalyzer(classDefs)
  } else {
    new IntraproceduralAnalyzer(new Array(0))
  }

  if (dump) wrapOutput { dumpClassDefs(classDefs) }

  if (className != null && methodName != null) {
    wrapOutput {
      println(
        (if (simpleCallGraph.classMap isDefinedAt className) {
           val cdp = simpleCallGraph.classMap(className)
           if (cdp.methodMap isDefinedAt methodName) {
             (cdp.methodMap(methodName).calls map {
                mdp => {
                  val m = if (mdp.method == null)
                            mdp.methodDef.method
                          else
                            mdp.method
                  javaTypeToName(m.classType) + "." + m.name
                }
              }).mkString(", ")
           } else {
             "No method " + methodName + " on class " + className
           }
         } else {
           "No class " + className
         }))
    }
  }

  if (intra) {
    intraproceduralAnalyzer.printTest
  }

  // Look, a real, if (very, very) simple, analyzsis
  val sourcesAndSinks = new SourceSinkMethodCallAnalyzer(configFile, simpleCallGraph)
  def printMethodsAndSources(mds: Set[MethodDef]) {
    mds foreach {
      (md) => println("  " + javaTypeToName(md.method.classType) + "." + md.name +
                (md.sourceLocation match {
                   case Some((fn,line,pos)) => " [" + fn + " at line: " + line + " pos: " + pos + "]"
                   case None => ""
                   }))
    }
  }
  wrapOutput {
    println("Methods that call sources (non-exhaustive): ")
    printMethodsAndSources(sourcesAndSinks.sources)
    println
    println("Methods that call sinks (non-exhaustive): ")
    printMethodsAndSources(sourcesAndSinks.sinks)
    println
    println("Methods that call other interesting methods (non-exhaustive): ")
    printMethodsAndSources(sourcesAndSinks.other)
    println
  }
}
