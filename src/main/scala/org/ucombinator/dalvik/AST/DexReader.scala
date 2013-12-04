package com.ucombinator.dalvik.AST

import collection.SortedMap
import annotation.tailrec
import language.implicitConversions
import java.io.{File, FileInputStream, DataInputStream, DataOutputStream, ByteArrayOutputStream, ByteArrayInputStream, StreamCorruptedException}
import java.nio.{ByteOrder, ByteBuffer}
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode._

class DexReader(ch:FileChannel) {
  private val reader = new LowLevelReaderHelper(ch)

  def this(is:FileInputStream) = this(is.getChannel)
  def this(fn:String) = this(new FileInputStream(fn))
  def this(f:File) = this(new FileInputStream(f))

  private class LowLevelReaderHelper(ch:FileChannel) {
    private val buffer = ch.map(READ_ONLY, 0, ch.size).order(ByteOrder.LITTLE_ENDIAN)

    private val biUshort = BigInt(0xffff)
    private val biUlong = (biUshort | biUshort << 16 | biUshort << 32 | biUshort << 48)


    private def byteToUByte(b:Byte):Short = { val s = b.toShort; if (b < 0) { (s & 0xff).toShort } else s }
    private def shortToUShort(s:Short):Int = { val i = s.toInt; if (s < 0) i & 0xffff else i }
    private def intToUInt(i:Int):Long = { val l = i.toLong; if (i < 0) (l << 32) >>> 32 else l }
    private def longToULong(l:Long):BigInt = { val b = BigInt(l); if (l < 0) b & biUlong else b }

    def order(bo:ByteOrder) = buffer.order(bo)
    def get(bytes:Array[Byte]) = buffer.get(bytes)
    def position():Int = buffer.position
    def position(x:Int) = buffer.position(x)
    def position(x:Long) = buffer.position(x.safeToInt)
    def checkPosition(x:Long) =
      if (buffer.position != x)
         throw new StreamCorruptedException("Expected to start read at position " +
           x + " but, stream is at position " + position)
    def readByte():Byte = buffer.get
    def readUByte():Short = byteToUByte(readByte())
    def readShort():Short = buffer.getShort
    def readUShort():Int = shortToUShort(readShort())
    def readInt():Int = buffer.getInt
    def readUInt():Long = intToUInt(readInt())
    def readLong():Long = buffer.getLong
    def readULong():BigInt = longToULong(readLong())

    def readUleb128():Long = {
      var num = 0
      var shift = 0
      var ubyte = 0
      do {
        ubyte = readUByte
        num |= (ubyte & 0x7f) << shift
        shift += 7
      } while((ubyte & 0x80) != 0)
      num
    }
  
    def readUleb128p1():Long = readUleb128 - 1
  
    def readSleb128():Long = {
      var num = 0
      var shift = 0
      var ubyte = 0
      do {
        ubyte = readUByte
        num |= (ubyte & 0x7f) << shift
        shift += 7
      } while((ubyte & 0x80) != 0)
      if ((ubyte & 0x40) == 0) num else num - (1 << shift)
    }
  
    /* translate between the android Modified-UTF-8 string, which starts with a
     * uleb128 size indicator and is followed by a null character, into a java
     * style Modified-UTF-8 string, which starts with a short to indicate the
     * length and has no null terminator. */
    def readUTF():String = {
      @tailrec
      def readUntilNull(i: Int, ls: Seq[Byte]): (Int, Seq[Byte]) = {
        val b = reader.readByte
        if (b == 0) (i, ls) else readUntilNull(i + 1, ls :+ b)
      }
      val size = readUleb128.safeToInt        // read the size in decoded characters
      val (bytesSize, bytes) = readUntilNull(0, Seq.empty[Byte])
      val ob = new ByteArrayOutputStream      // allocate output stream
      val dos = new DataOutputStream(ob)      // allocate a data outputstream
      dos.writeChar(bytesSize.toChar)         // write the start bytes
      for(b <- bytes) ob.write(b)             // write string bytes
      // finally create an input stream, and read the string
      val str = new DataInputStream(new ByteArrayInputStream(ob.toByteArray)).readUTF
      if (str.length != size) throw new Exception("expected " + size + " character string, but got " + str)
      str
    }
  }

  private class LongConverter(self:Long) {
    def safeToInt():Int =
      if (self.isValidInt)
         self.toInt
       else
         throw new ConversionOutOfRange(self + ":Long cannot be converted to an Int")
  }

  implicit private def longToLongConverter(l:Long):LongConverter = new LongConverter(l)

  case class ConversionOutOfRange(msg:String) extends Exception(msg)
  case class OutOfRange(msg:String) extends Exception(msg)

  /* constants */

  /* DEX file magic number (contains version number as well, so might need to be generalized) */
  private val DEX_FILE_MAGIC = Array(0x64, 0x65, 0x78, 0x0a, 0x30, 0x33, 0x35, 0x00)

  /* Constants indicating endianness of the file */
  private val ENDIAN_CONSTANT = 0x12345678
  private val REVERSE_ENDIAN_CONSTANT = 0x78563412

  /* Used to indicate when an index into the types or strings array is not valid */
  private val NO_INDEX = 0xffffffff
  private val NO_INDEX_LONG = 0xffffffffL

  /* value type indicators */
  private val VALUE_BYTE = 0x00
  private val VALUE_SHORT = 0x02
  private val VALUE_CHAR = 0x03
  private val VALUE_INT = 0x04
  private val VALUE_LONG = 0x06
  private val VALUE_FLOAT = 0x10
  private val VALUE_DOUBLE = 0x11
  private val VALUE_STRING = 0x17
  private val VALUE_TYPE = 0x18
  private val VALUE_FIELD = 0x19
  private val VALUE_METHOD = 0x1a
  private val VALUE_ENUM = 0x1b
  private val VALUE_ARRAY = 0x1c
  private val VALUE_ANNOTATION = 0x1d
  private val VALUE_NULL = 0x1e
  private val VALUE_BOOLEAN = 0x1f

  /* debugging state machine insturctions (not including extended instructions) */
  private val DBG_END_SEQUENCE = 0x00
  private val DBG_ADVANCE_PC = 0x01
  private val DBG_ADVANCE_LINE = 0x02
  private val DBG_START_LOCAL = 0x03
  private val DBG_START_LOCAL_EXTENDED = 0x04
  private val DBG_END_LOCAL = 0x05
  private val DBG_RESTART_LOCAL = 0x06
  private val DBG_SET_PROLOGUE_END = 0x07
  private val DBG_SET_EPILOGUE_BEGIN = 0x08
  private val DBG_SET_FILE = 0x09
  private val DBG_FIRST_SPECIAL = 0x0a  // the smallest special opcode
  private val DBG_LINE_BASE = -4        // the smallest line number increment
  private val DBG_LINE_RANGE = 15       // the number of line increments represented

  private val header = new Array[Long](20)
  private val signature = new Array[Byte](20)
  private var checksum = 0L
  private var strings:Array[String] = null
  private var types:Array[String] = null
  private var prototypes:Array[Prototype] = null
  private var fields:Array[Field] = null
  private var methods:Array[Method] = null
  private var classDefs:Array[ClassDef] = null
  private var typeMap = Map("V" -> VoidType, "Z" -> BooleanType,
    "B" -> ByteType, "S" -> ShortType, "C" -> CharType, "I" -> IntType,
    "J" -> LongType, "F" -> FloatType, "D" -> DoubleType)

  private object HeaderField extends Enumeration {
    type HeaderField = Value
    val FileSize, HeaderSize, EndianTag, LinkSize, LinkOffset, MapOffset,
        StringIdsSize, StringIdsOffset, TypeIdsSize, TypeIdsOffset,
        ProtoIdsSize, ProtoIdsOffset, FieldIdsSize, FieldIdsOffset,
        MethodIdsSize, MethodIdsOffset, ClassDefsSize, ClassDefsOffset,
        DataSize, DataOffset = Value
  }
  import HeaderField._

  private def readAndVerifyMagicNumber() {
    DEX_FILE_MAGIC.foreach(
      (x:Int) => {
         var ubyte = reader.readUByte;
         if (ubyte != x)
            throw new AssertionError("start of file does not match dex file number, expected " +
                                     BigInt(x).toString(16) + " but got " + BigInt(ubyte).toString(16))
      })
  }

  private var typeCallbacks = List.empty[(String, JavaType => Unit)]

  private def lookupType(idx:Int, callback:(JavaType) => Unit):JavaType = {
    lookupType(types(idx), callback)
  }

  private def lookupType(name:String, callback:((JavaType) => Unit) = null):JavaType = {
    if (typeMap isDefinedAt name) {
      typeMap(name)
    } else if (name(0) == '[') {
      val javaType = new ArrayType(null)
      if (callback != null)
         javaType.typeOf = lookupType(name.substring(1), ((t: JavaType) => javaType.typeOf = t))
      else
         javaType.typeOf = lookupType(name.substring(1))
      typeMap += name -> javaType
      javaType
    } else if (callback != null) {
      // when we have a callback, setup a callback to try to lookup the class
      // definition again, and replace the abstract type with the real type.
      typeCallbacks ::= (name, callback)
      new AbstractType(name)
    } else {
      // when there is no callback, use that abstract type to stand in for a
      // missing class definition.  These class definitions should be from
      // builtin libraries or libraries that will be loaded during runtime.
      val absType = new AbstractType(name)
      typeMap += name -> absType
      absType
    }
  }

  private def readSignature() = reader.get(signature)

  private def readHeader() {
    readAndVerifyMagicNumber
    checksum = reader.readUInt
    readSignature
    for(i <- 0 to 2) header(i) = reader.readUInt
    if (header(EndianTag.id) == REVERSE_ENDIAN_CONSTANT)
       reader.order(ByteOrder.BIG_ENDIAN)
    for(i <- 3 to 19) header(i) = reader.readUInt
  }

  private def readStrings() {
    val stringsSize = header(StringIdsSize.id).safeToInt
    strings = new Array[String](stringsSize)
    reader.checkPosition(header(StringIdsOffset.id))
    for(i <- 0 until stringsSize) {
      val offset = reader.readUInt
      val pos = reader.position
      reader.position(offset)
      strings(i) = reader.readUTF
      reader.position(pos)
    }
  }

  private def readTypes() {
    val typesSize = header(TypeIdsSize.id).safeToInt
    types = new Array[String](typesSize)
    reader.checkPosition(header(TypeIdsOffset.id))
    for(i <- 0 until typesSize) {
      val offset = reader.readUInt
      types(i) = strings(offset.safeToInt)
    }
  }
  
  private def maybeReadTypeList(off:Long):Array[JavaType] = {
    if (off == 0L) {
       null
    } else {
      val pos = reader.position
      reader.position(off)
      val size = reader.readUInt
      val typeList = new Array[JavaType](size.safeToInt)
      for(i <- 0 until size.safeToInt) {
        val typeIdx = reader.readUShort
        typeList(i) = lookupType(typeIdx, ((t: JavaType) => typeList(i) = t))
      }
      reader.position(pos)
      typeList
    }
  }

  private def readPrototypes() {
    val protosSize = header(ProtoIdsSize.id).safeToInt
    prototypes = new Array[Prototype](protosSize)
    reader.checkPosition(header(ProtoIdsOffset.id))
    for(i <- 0 until protosSize) {
      val shortyIdx = reader.readUInt
      val returnTypeIdx = reader.readUInt
      val parametersOff = reader.readUInt
      val parameters = maybeReadTypeList(parametersOff)
      
      prototypes(i) = new Prototype(strings(shortyIdx.safeToInt),
        lookupType(returnTypeIdx.safeToInt, ((t: JavaType) => prototypes(i).returnType = t)),
        parameters)
    }
  }

  private def readFields() {
    val fieldsSize = header(FieldIdsSize.id).safeToInt
    fields = new Array[Field](fieldsSize)
    reader.checkPosition(header(FieldIdsOffset.id))
    for(i <- 0 until fieldsSize) {
      val classIdx = reader.readUShort
      val typeIdx = reader.readUShort
      val nameIdx = reader.readUInt
      fields(i) = new Field(lookupType(classIdx, ((t: JavaType) => fields(i).classType = t)),
                            lookupType(typeIdx, ((t: JavaType) => fields(i).fieldType = t)),
                            strings(nameIdx.safeToInt))
    }
  }

  private def readMethods() {
    val methodsSize = header(MethodIdsSize.id).safeToInt
    methods = new Array[Method](methodsSize)
    reader.checkPosition(header(MethodIdsOffset.id))
    for(i <- 0 until methodsSize) {
      val classIdx = reader.readUShort
      val typeIdx = reader.readUShort
      val nameIdx = reader.readUInt
      methods(i) = new Method(lookupType(classIdx, ((t: JavaType) => methods(i).classType = t)),
          prototypes(typeIdx), strings(nameIdx.safeToInt))
    }
  }

  private def readAnnotationSetRefList(off:Long):Array[Array[AnnotationItem]] = {
    val pos = reader.position
    reader.position(off)
    val size = reader.readUInt.safeToInt
    val annotationSetRefList = new Array[Array[AnnotationItem]](size)
    for(i <- 0 until size)
      annotationSetRefList(i) = readAnnotationSetItem(reader.readUInt)
    reader.position(pos)
    annotationSetRefList
  }

  private def readAnnotationItem(off:Long):AnnotationItem = {
    val pos = reader.position
    reader.position(off)
    val visibility = reader.readUByte 
    val annotation = readEncodedAnnotation
    val annotationItem = new AnnotationItem(visibility, annotation)
    reader.position(pos)
    annotationItem
  }

  private def readAnnotationSetItem(off:Long):Array[AnnotationItem] = {
    val pos = reader.position
    reader.position(off)
    val size = reader.readUInt.safeToInt
    val annotations = new Array[AnnotationItem](size)
    for(i <- 0 until size) annotations(i) = readAnnotationItem(reader.readUInt)
    reader.position(pos)   
    annotations
  }

  private def maybeReadAnnotationSetItem(off:Long):Array[AnnotationItem] = 
    if (0L == off) null else readAnnotationSetItem(off)

  private def maybeReadAnnotationsDirectoryItem(off:Long):AnnotationsDirectoryItem = {
    if (0L == off) {
      null
    } else {
      val pos = reader.position
      reader.position(off)
      val classAnnotationOff = reader.readUInt
      val fieldsSize = reader.readUInt.safeToInt
      val annotatedMethodsSize = reader.readUInt.safeToInt
      val annotatedParametersSize = reader.readUInt.safeToInt
      val fieldAnnotations = new Array[FieldAnnotation](fieldsSize)
      val methodAnnotations = new Array[MethodAnnotation](annotatedMethodsSize)
      val parameterAnnotations = new Array[ParameterAnnotation](annotatedParametersSize)
      val classAnnotations = maybeReadAnnotationSetItem(classAnnotationOff)
      for(i <- 0 until fieldsSize) {
        val fieldIdx = reader.readUInt
        val annotationsOff = reader.readUInt
        val annotations = readAnnotationSetItem(annotationsOff)
        fieldAnnotations(i) = new FieldAnnotation(fields(fieldIdx.safeToInt), annotations)
      }
      for(i <- 0 until annotatedMethodsSize) {
        val methodIdx = reader.readUInt
        val annotationsOff = reader.readUInt
        val annotations = readAnnotationSetItem(annotationsOff)
        methodAnnotations(i) = new MethodAnnotation(methods(methodIdx.safeToInt), annotations)
      }
      for(i <- 0 until annotatedParametersSize) {
        val methodIdx = reader.readUInt
        val annotationsOff = reader.readUInt
        val annotations = readAnnotationSetRefList(annotationsOff)
        parameterAnnotations(i) = new ParameterAnnotation(methods(methodIdx.safeToInt), annotations)
      }
      reader.position(pos)
      new AnnotationsDirectoryItem(classAnnotations, fieldAnnotations, methodAnnotations, parameterAnnotations)
    }
  }

  private def readEncodedFields(size: Int, staticValues: Array[EncodedValue]): Array[FieldDef] = {
    val fieldArray = new Array[FieldDef](size)
    var fieldIdx = 0
    def getStaticValue(i: Int, t: JavaType, accessFlags: Long): EncodedValue = {
      if ((accessFlags & AccessFlags.ACC_STATIC) == 0) {
        null
      } else if (staticValues != null && staticValues.isDefinedAt(i)) {
        staticValues(i)
      } else {
        t match {
          case BooleanType => EncodedBoolean(false)
          case ByteType    => EncodedByte(0.toByte)
          case ShortType   => EncodedShort(0.toShort) 
          case CharType    => EncodedChar(0.toChar)
          case IntType     => EncodedInt(0)
          case LongType    => EncodedLong(0L)
          case FloatType   => EncodedFloat(0.0.toFloat)
          case DoubleType  => EncodedDouble(0.0)
          case _           => EncodedNull
        }
      }
    }
    for(i <- 0 until size) {
      fieldIdx += reader.readUleb128.safeToInt
      val accessFlags = reader.readUleb128
      val field = fields(fieldIdx)
      val staticVal = getStaticValue(i, field.fieldType, accessFlags)
      fieldArray(i) = new FieldDef(field, accessFlags, staticVal)
    }
    fieldArray
  }
  
  private def readPackedSwitchPayload():(Int, PackedSwitchPayload) = {
    val size = reader.readUShort
    val targets = new Array[Int](size)
    val firstKey = reader.readInt
    for(i <- 0 until size) targets(i) = reader.readInt
    ((size * 2) + 4, new PackedSwitchPayload(firstKey, targets))
  }

  private def readSparseSwitchPayload():(Int, SparseSwitchPayload) = {
    val size = reader.readUShort
    val keys = new Array[Int](size)
    val targets = new Array[Int](size)
    for(i <- 0 until size) keys(i) = reader.readInt
    for(i <- 0 until size) targets(i) = reader.readInt
    ((size * 4) + 2, new SparseSwitchPayload(keys, targets))
  }

  private def readFillArrayDataPayload():(Int, FillArrayDataPayload) = {
    val elementWidth = reader.readUShort
    val size = reader.readUInt
    val byteCount = size.safeToInt * elementWidth
    val totalShorts = (((size * elementWidth + 1) / 2) + 4).safeToInt
    val data = new Array[Short](size.safeToInt * elementWidth)
    for(i <- 0 until byteCount) data(i) = reader.readUByte
    for(i <- 0 until ((totalShorts * 2) - (8 + byteCount))) reader.readUByte // throw away padding.
    (totalShorts, new FillArrayDataPayload(size, elementWidth, data))
  }

  private def readInstructionFormat00op(opcode:Short):(Int, Instruction) = {
    val b = reader.readUByte
    if (opcode == 0x00) {
      if (b == 0x01) {
        readPackedSwitchPayload()
      } else if (b == 0x02) {
        readSparseSwitchPayload()
      } else if (b == 0x03) {
        readFillArrayDataPayload()
      } else if (b == 0) {
        (1, Nop)
      } else {
        throw new Exception("Expected 0 byte in 00|op format: " + b)
      }
    } else if (b != 0) {
      throw new Exception("Expected 0 byte in 00|op format: " + b)
    } else {
      (1, opcode match {
            case 0x00 => Nop
            case 0x0e => ReturnVoid
            case num if 0x3e to 0x43 contains num => throw new Exception("Unexpected opcode in unused range: " + opcode)
            case 0x73 => throw new Exception("Unexpected opcode in unused range: " + opcode)
            case 0x79 => throw new Exception("Unexpected opcode in unused range: " + opcode)
            case 0x7a => throw new Exception("Unexpected opcode in unused range: " + opcode)
            case num if 0xe3 to 0xff contains num => throw new Exception("Unexpected opcode in unused range: " + opcode)
            case _ => throw new Exception("Received opcode for unknown format in 00|op format: " + opcode)
          })
    }
  }

  private def readInstructionFormatBAop(opcode:Short):Instruction = {
    val BA = reader.readUByte
    val B = (BA >>> 4).toByte
    val A = (BA & 0x0f).toByte
    opcode match {
      case 0x01 => Move(A, B)
      case 0x04 => MoveWide(A, B)
      case 0x07 => MoveObject(A, B)
      case 0x12 => Const4(A, ((B << 28) >> 28)) // sign extend the constant to 32-bits
      case 0x21 => ArrayLength(A, B)
      case 0x7b => NegInt(A, B)
      case 0x7c => NotInt(A, B)
      case 0x7d => NegLong(A, B)
      case 0x7e => NotLong(A, B)
      case 0x7f => NegFloat(A, B)
      case 0x80 => NegDouble(A, B)
      case 0x81 => IntToLong(A, B)
      case 0x82 => IntToFloat(A, B)
      case 0x83 => IntToDouble(A, B)
      case 0x84 => LongToInt(A, B)
      case 0x85 => LongToFloat(A, B)
      case 0x86 => LongToDouble(A, B)
      case 0x87 => FloatToInt(A, B)
      case 0x88 => FloatToLong(A, B)
      case 0x89 => FloatToDouble(A, B)
      case 0x8a => DoubleToInt(A, B)
      case 0x8b => DoubleToLong(A, B)
      case 0x8c => DoubleToFloat(A, B)
      case 0x8d => IntToByte(A, B)
      case 0x8e => IntToChar(A, B)
      case 0x8f => IntToShort(A, B)
      case 0xb0 => AddInt2Addr(A, B)
      case 0xb1 => SubInt2Addr(A, B)
      case 0xb2 => MulInt2Addr(A, B)
      case 0xb3 => DivInt2Addr(A, B)
      case 0xb4 => RemInt2Addr(A, B)
      case 0xb5 => AndInt2Addr(A, B)
      case 0xb6 => OrInt2Addr(A, B)
      case 0xb7 => XorInt2Addr(A, B)
      case 0xb8 => ShlInt2Addr(A, B)
      case 0xb9 => ShrInt2Addr(A, B)
      case 0xba => UshrInt2Addr(A, B)
      case 0xbb => AddLong2Addr(A, B)
      case 0xbc => SubLong2Addr(A, B)
      case 0xbd => MulLong2Addr(A, B)
      case 0xbe => DivLong2Addr(A, B)
      case 0xbf => RemLong2Addr(A, B)
      case 0xc0 => AndLong2Addr(A, B)
      case 0xc1 => OrLong2Addr(A, B)
      case 0xc2 => XorLong2Addr(A, B)
      case 0xc3 => ShlLong2Addr(A, B)
      case 0xc4 => ShrLong2Addr(A, B)
      case 0xc5 => UshrLong2Addr(A, B)
      case 0xc6 => AddFloat2Addr(A, B)
      case 0xc7 => SubFloat2Addr(A, B)
      case 0xc8 => MulFloat2Addr(A, B)
      case 0xc9 => DivFloat2Addr(A, B)
      case 0xca => RemFloat2Addr(A, B)
      case 0xcb => AddDouble2Addr(A, B)
      case 0xcc => SubDouble2Addr(A, B)
      case 0xcd => MulDouble2Addr(A, B)
      case 0xce => DivDouble2Addr(A, B)
      case 0xcf => RemDouble2Addr(A, B)
      case _ => throw new Exception("Received opcode for unknown format in B|A|op format: " + opcode)
    }
  }

  private def readInstructionFormatAAop(opcode:Short):Instruction = {
    val AA = reader.readUByte
    opcode match {
      case 0x0a => MoveResult(AA)
      case 0x0b => MoveResultWide(AA)
      case 0x0c => MoveResultObject(AA)
      case 0x0d => MoveException(AA)
      case 0x0f => Return(AA)
      case 0x10 => ReturnWide(AA)
      case 0x11 => ReturnObject(AA)
      case 0x1d => MonitorEnter(AA)
      case 0x1e => MonitorExit(AA)
      case 0x27 => Throw(AA)
      case 0x28 => Goto((AA << 24) >> 24) // sign extend to integer
      case _ => throw new Exception("Received opcode for unknown format in AA|op format: " + opcode)
    }
  }

  private def readInstructionFormat00op_AAAA(opcode:Short):Instruction = {
    val nullByte = reader.readUByte
    if (nullByte != 0) throw new Exception("Expected 0 byte in 00|op AAAA format but got: " + nullByte)
    val AAAA = reader.readShort // read a signed short so we don't need to bother with sign extending
    opcode match {
      case 0x29 => Goto16(AAAA.toInt)
      case _ => throw new Exception("Received opcode for unknown format in 00|op AAAA format: " + opcode)
    }
  }

  private def readInstructionFormatAAop_BBBB(opcode:Short):Instruction = {
    val AA = reader.readUByte
    val BBBB = reader.readUShort
    opcode match {
      case 0x02 => MoveFrom16(AA, BBBB)
      case 0x05 => MoveWideFrom16(AA, BBBB)
      case 0x08 => MoveObjectFrom16(AA, BBBB)
      case 0x13 => Const16(AA, (BBBB << 16) >> 16) // sign extend to 32-bit constant
      case 0x15 => ConstHigh16(AA, (BBBB << 16)) // right-zero extend constant
      case 0x16 => ConstWide16(AA, (BBBB.toLong << 48) >> 48) // sign extend to 64-bit constant
      case 0x19 => ConstWideHigh16(AA, BBBB.toLong << 48) // right-zero extend constant
      case 0x1a => ConstString(AA, strings(BBBB)) // lookup the string from the constants pool
      case 0x1c => {
        val ins = ConstClass(AA, null)
        // lookup the type from the constants pool, and setup a callback if it is not yet available
        ins.b = lookupType(BBBB, ((t: JavaType) => ins.b = t))
        ins
      }
      case 0x1f => {
        val ins = CheckCast(AA, null)
        // lookup the type from the constants pool, and setup a callback if it is not yet available
        ins.b = lookupType(BBBB, ((t: JavaType) => ins.b = t))
        ins
      }
      case 0x22 => {
        val ins = NewInstance(AA, null)
        // lookup the type from the constants pool, and setup a callback if it is not yet available
        ins.b = lookupType(BBBB, ((t: JavaType) => ins.b = t))
        ins
      }
      case 0x38 => IfEqz(AA, (BBBB << 16) >> 16) // sign extend target
      case 0x39 => IfNez(AA, (BBBB << 16) >> 16) // sign extend target
      case 0x3a => IfLtz(AA, (BBBB << 16) >> 16) // sign extend target
      case 0x3b => IfGez(AA, (BBBB << 16) >> 16) // sign extend target
      case 0x3c => IfGtz(AA, (BBBB << 16) >> 16) // sign extend target
      case 0x3d => IfLez(AA, (BBBB << 16) >> 16) // sign extend target
      case 0x60 => SGet(AA, fields(BBBB)) // lookup field from constants pool
      case 0x61 => SGetWide(AA, fields(BBBB)) // lookup field from constants pool
      case 0x62 => SGetObject(AA, fields(BBBB)) // lookup field from constants pool
      case 0x63 => SGetBoolean(AA, fields(BBBB)) // lookup field from constants pool
      case 0x64 => SGetByte(AA, fields(BBBB)) // lookup field from constants pool
      case 0x65 => SGetChar(AA, fields(BBBB)) // lookup field from constants pool
      case 0x66 => SGetShort(AA, fields(BBBB)) // lookup field from constants pool
      case 0x67 => SPut(AA, fields(BBBB)) // lookup field from constants pool
      case 0x68 => SPutWide(AA, fields(BBBB)) // lookup field from constants pool
      case 0x69 => SPutObject(AA, fields(BBBB)) // lookup field from constants pool
      case 0x6a => SPutBoolean(AA, fields(BBBB)) // lookup field from constants pool
      case 0x6b => SPutByte(AA, fields(BBBB)) // lookup field from constants pool
      case 0x6c => SPutChar(AA, fields(BBBB)) // lookup field from constants pool
      case 0x6d => SPutShort(AA, fields(BBBB)) // lookup field from constants pool
      case _ => throw new Exception("Received opcode for unknown format in AA|op BBBB format: " + opcode)
    }
  }

  private def readInstructionFormatAAop_CCBB(opcode:Short):Instruction = {
    val AA = reader.readUByte
    val BB = reader.readUByte
    val CC = reader.readUByte
    opcode match {
      case 0x2d => CmplFloat(AA, BB, CC)
      case 0x2e => CmpgFloat(AA, BB, CC)
      case 0x2f => CmplDouble(AA, BB, CC)
      case 0x30 => CmpgDouble(AA, BB, CC)
      case 0x31 => CmpLong(AA, BB, CC)
      case 0x44 => AGet(AA, BB, CC)
      case 0x45 => AGetWide(AA, BB, CC)
      case 0x46 => AGetObject(AA, BB, CC)
      case 0x47 => AGetBoolean(AA, BB, CC)
      case 0x48 => AGetByte(AA, BB, CC)
      case 0x49 => AGetChar(AA, BB, CC)
      case 0x4a => AGetShort(AA, BB, CC)
      case 0x4b => APut(AA, BB, CC)
      case 0x4c => APutWide(AA, BB, CC)
      case 0x4d => APutObject(AA, BB, CC)
      case 0x4e => APutBoolean(AA, BB, CC)
      case 0x4f => APutByte(AA, BB, CC)
      case 0x50 => APutChar(AA, BB, CC)
      case 0x51 => APutShort(AA, BB, CC)
      case 0x90 => AddInt(AA, BB, CC)
      case 0x91 => SubInt(AA, BB, CC)
      case 0x92 => MulInt(AA, BB, CC)
      case 0x93 => DivInt(AA, BB, CC)
      case 0x94 => RemInt(AA, BB, CC)
      case 0x95 => AndInt(AA, BB, CC)
      case 0x96 => OrInt(AA, BB, CC)
      case 0x97 => XorInt(AA, BB, CC)
      case 0x98 => ShlInt(AA, BB, CC)
      case 0x99 => ShrInt(AA, BB, CC)
      case 0x9a => UshrInt(AA, BB, CC)
      case 0x9b => AddLong(AA, BB, CC)
      case 0x9c => SubLong(AA, BB, CC)
      case 0x9d => MulLong(AA, BB, CC)
      case 0x9e => DivLong(AA, BB, CC)
      case 0x9f => RemLong(AA, BB, CC)
      case 0xa0 => AndLong(AA, BB, CC)
      case 0xa1 => OrLong(AA, BB, CC)
      case 0xa2 => XorLong(AA, BB, CC)
      case 0xa3 => ShlLong(AA, BB, CC)
      case 0xa4 => ShrLong(AA, BB, CC)
      case 0xa5 => UshrLong(AA, BB, CC)
      case 0xa6 => AddFloat(AA, BB, CC)
      case 0xa7 => SubFloat(AA, BB, CC)
      case 0xa8 => MulFloat(AA, BB, CC)
      case 0xa9 => DivFloat(AA, BB, CC)
      case 0xaa => RemFloat(AA, BB, CC)
      case 0xab => AddDouble(AA, BB, CC)
      case 0xac => SubDouble(AA, BB, CC)
      case 0xad => MulDouble(AA, BB, CC)
      case 0xae => DivDouble(AA, BB, CC)
      case 0xaf => RemDouble(AA, BB, CC)
      case 0xd8 => AddIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xd9 => RsubIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xda => MulIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xdb => DivIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xdc => RemIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xdd => AndIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xde => OrIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xdf => XorIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xe0 => ShlIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xe1 => ShrIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case 0xe2 => UshrIntLit8(AA, BB, (CC << 28) >> 28) // sign extend to 32-bit constant
      case _ => throw new Exception("Received opcode for unknown format in AA|op CC|BB format: " + opcode)
    }
  }

  private def readInstructionFormatBAop_CCCC(opcode:Short):Instruction = {
    val BA = reader.readUByte
    val B = (BA >>> 4).toByte
    val A = (BA & 0x0f).toByte
    val CCCC = reader.readUShort
    opcode match {
      case 0x20 => {
        val ins = InstanceOf(A, B, null)
        // lookup type in constants pool, and setup a callback if it is not yet available
        ins.c = lookupType(CCCC, ((t: JavaType) => ins.c = t))
        ins
      }
      case 0x23 => {
        val ins = NewArray(A, B, null)
        // lookup type in constants pool, and setup a callback if it is not yet available
        ins.c = lookupType(CCCC, ((t: JavaType) => ins.c = t))
        ins
      }
      case 0x32 => IfEq(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0x33 => IfNe(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0x34 => IfLt(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0x35 => IfGe(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0x36 => IfGt(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0x37 => IfLe(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0x52 => IGet(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x53 => IGetWide(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x54 => IGetObject(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x55 => IGetBoolean(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x56 => IGetByte(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x57 => IGetChar(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x58 => IGetShort(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x59 => IPut(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x5a => IPutWide(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x5b => IPutObject(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x5c => IPutBoolean(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x5d => IPutByte(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x5e => IPutChar(A, B, fields(CCCC)) // lookup field in constants pool
      case 0x5f => IPutShort(A, B, fields(CCCC)) // lookup field in constants pool
      case 0xd0 => AddIntLit16(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0xd1 => RsubInt(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0xd2 => MulIntLit16(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0xd3 => DivIntLit16(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0xd4 => RemIntLit16(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0xd5 => AndIntLit16(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0xd6 => OrIntLit16(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case 0xd7 => XorIntLit16(A, B, (CCCC << 16) >> 16) // sign extend to 32-bit constant
      case _ => throw new Exception("Received opcode for unknown format in B|A|op CCCC format: " + opcode)
    }
  }

  private def readInstructionFormat00op_AAAA_AAAA(opcode:Short):Instruction = {
    val nullByte = reader.readUByte
    if (nullByte != 0) throw new Exception("Expected 0 byte in 00|op AAAA AAAA format but got: " + nullByte + " with opcode: " + opcode)
    val AAAA_AAAA = reader.readInt
    opcode match {
      case 0x2a => Goto32(AAAA_AAAA)
      case _ => throw new Exception("Received opcode for unknown format in 00|op AAAA_lo AAAA_hi format: " + opcode)
    }
  }

  private def readInstructionFormat00op_AAAA_BBBB(opcode:Short):Instruction = {
    val nullByte = reader.readUByte
    if (nullByte != 0) throw new Exception("Expected 0 byte in 00|op AAAA BBBB format but got: " + nullByte + " with opcode: " + opcode)
    val AAAA = reader.readUShort
    val BBBB = reader.readUShort
    opcode match {
      case 0x03 => Move16(AAAA, BBBB)
      case 0x06 => MoveWide16(AAAA, BBBB)
      case 0x09 => MoveObject16(AAAA, BBBB)
      case _ => throw new Exception("Received opcode for unknown format in 00|op AAAA BBBB format: " + opcode)
    }
  }

  private def readInstructionFormatAAop_BBBB_BBBB(opcode:Short):Instruction = {
    val AA = reader.readUByte
    val BBBB_BBBB = reader.readUInt
    opcode match {
      case 0x14 => Const(AA, BBBB_BBBB.toInt) // convert from unsigned int (Long) to signed int
      case 0x17 => ConstWide32(AA, (BBBB_BBBB << 32) >> 32) // sign extend to 64-bit
      case 0x1b => ConstStringJumbo(AA, strings(BBBB_BBBB.safeToInt))
      case 0x26 => TempFillArrayData(AA, BBBB_BBBB.toInt) // convert from unsigned int (Long) to signed int
      case 0x2b => TempPackedSwitch(AA, BBBB_BBBB.toInt) // convert from unsigned int (Long) to signed int
      case 0x2c => TempSparseSwitch(AA, BBBB_BBBB.toInt) // convert from unsigned int (Long) to signed int
      case _ => throw new Exception("Received opcode for unknown format in AA|op BBBB_lo BBBB_hi format: " + opcode)
    }
  }

  private def readInstructionFormatAGop_BBBB_FEDC(opcode:Short):Instruction = {
    val AG = reader.readUByte
    val A = (AG >>> 4).toByte
    val G = (AG & 0x0f).toByte
    val BBBB = reader.readUShort
    val DC = reader.readUByte
    val D = (DC >>> 4).toByte
    val C = (DC & 0x0f).toByte
    val FE = reader.readUByte
    val F = (FE >>> 4).toByte
    val E = (FE & 0x0f).toByte
    val refs = A match {
                 case 0 => Array[Byte]()
                 case 1 => Array(C)
                 case 2 => Array(C, D)
                 case 3 => Array(C, D, E)
                 case 4 => Array(C, D, E, F)
                 case 5 => Array(C, D, E, F, G)
                 case _ => throw new Exception("Recieved larger count that available registers")
               }

    opcode match {
      case 0x24 => {
        val ins = FilledNewArray(refs, null)
        ins.b = lookupType(BBBB, ((t: JavaType) => ins.b = t))
        ins
      }
      case 0x6e => InvokeVirtual(refs, methods(BBBB))
      case 0x6f => InvokeSuper(refs, methods(BBBB))
      case 0x70 => InvokeDirect(refs, methods(BBBB))
      case 0x71 => InvokeStatic(refs, methods(BBBB))
      case 0x72 => InvokeInterface(refs, methods(BBBB))
      case _ => throw new Exception("Received opcode for unknown format in A|G|op BBBB F|E|D|C format: " + opcode)
    }
  }

  private def readInstructionFormatAAop_BBBB_CCCC(opcode:Short):Instruction = {
    val AA = reader.readUByte
    val BBBB = reader.readUShort
    val CCCC = reader.readUShort
    opcode match {
      case 0x25 => {
        val ins = FilledNewArrayRange(CCCC, AA, null)
        ins.b = lookupType(BBBB, ((t: JavaType) => ins.b = t))
        ins
      }
      case 0x74 => InvokeVirtualRange(CCCC, AA, methods(BBBB))
      case 0x75 => InvokeSuperRange(CCCC, AA, methods(BBBB))
      case 0x76 => InvokeDirectRange(CCCC, AA, methods(BBBB))
      case 0x77 => InvokeStaticRange(CCCC, AA, methods(BBBB))
      case 0x78 => InvokeInterfaceRange(CCCC, AA, methods(BBBB))
      case _ => throw new Exception("Received opcode for unknown format in AA|op BBBB CCCC format: " + opcode)
    }
  }

  private def readInstructionFormatAAop_BBBB_BBBB_BBBB_BBBB(opcode:Short):Instruction = {
    val AA = reader.readUByte
    val BBBB_BBBB_BBBB_BBBB = reader.readLong
    opcode match {
      case 0x18 => ConstWide(AA, BBBB_BBBB_BBBB_BBBB)
      case _ => throw new Exception("Received opcode for unknown format in AA|op BBBB BBBB BBBB BBBB format: " + opcode)
    }
  }

  private def readInstruction():(Int,Instruction) = {
    val opcode = reader.readUByte
    if (opcode == 0x00 || opcode == 0x0e || (opcode >= 0x3e && opcode <= 0x43) || opcode == 0x73 || opcode == 0x79 || opcode == 0x7a || (opcode >= 0xe3 && opcode <= 0xff)) {
      readInstructionFormat00op(opcode)
    } else if (opcode == 0x01 || opcode == 0x04 || opcode == 0x07 || opcode == 0x12 || opcode == 0x21 || (opcode >= 0x7b && opcode <= 0x8f) || (opcode >= 0xb0 && opcode <= 0xcf)) {
      (1, readInstructionFormatBAop(opcode))
    } else if ((opcode >= 0x0a && opcode <= 0x0d) || (opcode >= 0x0f && opcode <= 0x11) || opcode == 0x1d || opcode == 0x1e || opcode == 0x27 || opcode == 0x28) {
      (1, readInstructionFormatAAop(opcode))
    } else if (opcode == 0x29) {
      (2, readInstructionFormat00op_AAAA(opcode))
    } else if (opcode == 0x02 || opcode == 0x05 || opcode == 0x08 || opcode == 0x13 || opcode == 0x15 || opcode == 0x16 || opcode == 0x19 || opcode == 0x1a || opcode == 0x1c || opcode == 0x1f || opcode == 0x22 || (opcode >= 0x38 && opcode <= 0x3d) || (opcode >= 0x60 && opcode <= 0x6d)) {
      (2, readInstructionFormatAAop_BBBB(opcode))
    } else if ((opcode >= 0x2d && opcode <= 0x31) || (opcode >= 0x44 && opcode <= 0x51) || (opcode >= 0x90 && opcode <= 0xaf) || (opcode >= 0xd8 && opcode <= 0xe2)) {
      (2, readInstructionFormatAAop_CCBB(opcode))
    } else if (opcode == 0x20 || opcode == 0x23 || (opcode >= 0x32 && opcode <= 0x37) || (opcode >= 0x52 && opcode <= 0x5f) || (opcode >= 0xd0 && opcode <= 0xd7)) {
      (2, readInstructionFormatBAop_CCCC(opcode))
    } else if (opcode == 0x2a) {
      (3, readInstructionFormat00op_AAAA_AAAA(opcode))
    } else if (opcode == 0x03 || opcode == 0x06 || opcode == 0x09) {
      (3, readInstructionFormat00op_AAAA_BBBB(opcode))
    } else if (opcode == 0x14 || opcode == 0x17 || opcode == 0x1b || opcode == 0x26 || opcode == 0x2b || opcode == 0x2c) {
      (3, readInstructionFormatAAop_BBBB_BBBB(opcode))
    } else if (opcode == 0x24 || (opcode >= 0x6e && opcode <= 0x72)) {
      (3, readInstructionFormatAGop_BBBB_FEDC(opcode))
    } else if (opcode == 0x25 || (opcode >= 0x74 && opcode <= 0x78)) {
      (3, readInstructionFormatAAop_BBBB_CCCC(opcode))
    } else if (opcode == 0x18) {
      (5, readInstructionFormatAAop_BBBB_BBBB_BBBB_BBBB(opcode))
    } else {
      throw new Exception("Encountered unhandled opcode: " + opcode.toInt.toHexString)
    }
  }

  private def readInstructions(shortCount:Int, debugInfo:DebugInfo):SortedMap[Int,Instruction] = {
    var insns = SortedMap.empty[Int,Instruction]
    var shortOffset = 0
    var debugTable = if (debugInfo == null) Map.empty[Long,SourceInfo] else debugInfo.debugTable
    while(shortOffset < shortCount) {
      val (shortsRead, instruction) = readInstruction
      if (debugTable isDefinedAt shortOffset.toLong)
        instruction.sourceInfo = debugTable(shortOffset.toLong)
      insns += shortOffset -> instruction
      shortOffset += shortsRead
    }
    insns
    // TODO: patch up instructions (possibly with TryItems and DebugInfo)
  }

  private def readTryItem():TryItem = {
    val startAddr = reader.readUInt
    val insnCount = reader.readUShort
    val handlerOff = reader.readUShort
    new TryItem(startAddr, insnCount, handlerOff)
  }

  private def readEncodedCatchHandlerList():Array[EncodedCatchHandler] = {
    val startPos = reader.position
    val size = reader.readUleb128
    val list = new Array[EncodedCatchHandler](size.safeToInt)
    for(i <- 0L until size) list(i.safeToInt) = readEncodedCatchHandler(reader.position - startPos)
    list
  }

  private def readEncodedCatchHandler(offset:Int):EncodedCatchHandler = {
    val size = reader.readSleb128
    val isize = size.abs.safeToInt
    val handlers = new Array[CatchHandler](isize)
    for(i <- 0 until isize) {
      val typeIdx = reader.readUleb128
      val addr = reader.readUleb128
      handlers(i) = new CatchHandler(
        lookupType(typeIdx.safeToInt,
                   ((t: JavaType) => handlers(i).exceptionType = t)),
        addr)
    }
    new EncodedCatchHandler(offset, handlers, if (size <= 0) reader.readUleb128 else -1)
  }

  class UnrecognizedDebugCodeItemException(msg:String) extends Exception(msg)

  private def maybeReadDebugInfo(off:Long, sourceFile:String, parameterTypes:Array[JavaType]):DebugInfo = {
    if (0L == off) {
      null
    } else {
      val pos = reader.position
      reader.position(off)
      val lineStart = reader.readUleb128
      val parametersSize = reader.readUleb128
      val parameterNames = new Array[String](parametersSize.safeToInt)
      var varTable = Map.empty[Long, VarInfo]
      var allVarTable = Map.empty[Long, VarInfo]
      for (i <- 0 until parametersSize.safeToInt) {
        val idx = reader.readUleb128p1
        val name = if (idx == NO_INDEX) null else strings(idx.safeToInt)
        parameterNames(i) = name
        val varInfo = new VarInfo(i, name, parameterTypes(i), null)
        varTable += i.toLong -> varInfo
        allVarTable += i.toLong -> varInfo
      }

      var infoTable = Map.empty[Long, SourceInfo]
      var opcode = reader.readUByte
      var address = 0L
      var fn = sourceFile
      var line = lineStart
      var prologue_end = false
      var epilogue_begin = false

      while(opcode != DBG_END_SEQUENCE) {
        opcode match {
          case DBG_ADVANCE_PC => address += reader.readUleb128
          case DBG_ADVANCE_LINE => line += reader.readSleb128
          case DBG_START_LOCAL => {
            val registerNum = reader.readUleb128
            val nameIdx = reader.readUleb128p1
            val typeIdx = reader.readUleb128p1
            val varInfo:VarInfo = new VarInfo(registerNum,
                (if (nameIdx == NO_INDEX) null else strings(nameIdx.safeToInt)),
                null, null)
            if (typeIdx != NO_INDEX)
               varInfo.varType = lookupType(typeIdx.safeToInt, ((t: JavaType) => varInfo.varType = t))
            varTable += registerNum -> varInfo
            allVarTable += registerNum -> varInfo
          }
          case DBG_START_LOCAL_EXTENDED => {
            val registerNum = reader.readUleb128
            val nameIdx = reader.readUleb128p1
            val typeIdx = reader.readUleb128p1
            val sigIdx = reader.readUleb128p1
            val varInfo:VarInfo = new VarInfo(registerNum,
                (if (nameIdx == NO_INDEX) null else strings(nameIdx.safeToInt)),
                null, (if (sigIdx == NO_INDEX) null else strings(sigIdx.safeToInt)))
            if (typeIdx != NO_INDEX)
              varInfo.varType = lookupType(typeIdx.safeToInt, ((t: JavaType) => varInfo.varType = t))
            varTable += registerNum -> varInfo
            allVarTable += registerNum -> varInfo
          }
          case DBG_END_LOCAL => {
            val registerNum = reader.readUleb128
            varTable -= registerNum
          }
          case DBG_RESTART_LOCAL => {
            val registerNum = reader.readUleb128
            if (allVarTable isDefinedAt registerNum)
              varTable += registerNum -> allVarTable(registerNum)
          }
          case DBG_SET_PROLOGUE_END => prologue_end = true
          case DBG_SET_EPILOGUE_BEGIN => epilogue_begin = true
          case DBG_SET_FILE => {
            val idx = reader.readUleb128p1
            fn = if (idx == NO_INDEX) null else strings(idx.safeToInt)
          }
          case _ => {
            val adjusted_opcode = opcode - DBG_FIRST_SPECIAL
            line += DBG_LINE_BASE + (adjusted_opcode % DBG_LINE_RANGE)
            address += (adjusted_opcode / DBG_LINE_RANGE)
            infoTable += address -> new SourceInfo(address, line, fn, varTable,
                !prologue_end, epilogue_begin)
            prologue_end = false
            epilogue_begin = false
          }
        }
        opcode = reader.readUByte
      }
      reader.position(pos)
      new DebugInfo(lineStart,parameterNames,infoTable)
    }
  }

  private var counter = 0

  abstract class TryMark
  case object TryStart extends TryMark
  case object TryEnd extends TryMark
  case class Catch(typeName:String) extends TryMark
  case object CatchAll extends TryMark

  private def maybeReadCodeItem(off:Long, sourceFile:String, parameterTypes:Array[JavaType], classType:JavaType, methodName:String):CodeItem = {
    if (0 == off) {
      null
    } else {
      counter += 1
      val pos = reader.position
      reader.position(off)
      val registersSize = reader.readUShort
      val insSize = reader.readUShort
      val outsSize = reader.readUShort
      val triesSize = reader.readUShort
      val debugInfoOff = reader.readUInt
      val debugInfo = maybeReadDebugInfo(debugInfoOff, sourceFile, parameterTypes)
      val insnsSize = reader.readUInt.safeToInt
      val insns = readInstructions(insnsSize, debugInfo)
      if ((insnsSize % 2) == 1 && triesSize > 0) reader.readUShort
      val tries = new Array[TryItem](triesSize)
      for (i <- 0 until triesSize) tries(i) = readTryItem
      val handlers = if (triesSize > 0) readEncodedCatchHandlerList else null
      reader.position(pos)
      var tryMap = SortedMap.empty[Long,TryMark]

      var updatedInsnsMap = SortedMap.empty[Int,Instruction]
      var idx = 0
      var addrToIndex = Map.empty[Int,Int]
      
      for(t <- insns) {
        t._2 match {
          case TempPackedSwitch(a, b) => {
            val address = t._1 + b
            if (insns isDefinedAt address) {
              insns(address) match {
                case PackedSwitchPayload(firstKey, targets) => updatedInsnsMap += t._1 -> PackedSwitch(a, firstKey, targets)
                case _ => throw new Exception("Expected to find packed-switch-payload at " + address + ", but instead found " + insns(address))
              }
            } else {
              throw new Exception("Expected to find packed-switch-payload at " + address + ", but there is no instruction at this address")
            }
            addrToIndex += t._1 -> idx
            idx += 1
          }
          case TempSparseSwitch(a, b) =>  {
            val address = t._1 + b
            if (insns isDefinedAt address) {
              insns(address) match {
                case SparseSwitchPayload(keys, targets) => updatedInsnsMap += t._1 -> SparseSwitch(a, keys, targets)
                case _ => throw new Exception("Expected to find sparse-switch-payload at " + address + ", but instead found " + insns(address))
              }
            } else {
              throw new Exception("Expected to find sparse-switch-payload at " + address + ", but there is no instruction at this address")
            }
            addrToIndex += t._1 -> idx
            idx += 1
          }
          case TempFillArrayData(a, b) => {
            val address = t._1 + b
            if (insns isDefinedAt address) {
              insns(address) match {
                case FillArrayDataPayload(size, elementWidth, data) => updatedInsnsMap += t._1 -> FillArrayData(a, size, elementWidth, data)
                case _ => throw new Exception("Expected to find file-array-data-payload at " + address + ", but instead found " + insns(address))
              }
            } else {
              throw new Exception("Expected to find file-array-data-payload at " + address + ", but there is no instruction at this address")
            }
            addrToIndex += t._1 -> idx
            idx += 1
          }
          case PackedSwitchPayload(firstKey, targets) => ()
          case SparseSwitchPayload(keys, targets) => ()
          case FillArrayDataPayload(size, elementWidth, data) => ()
          case _ => { updatedInsnsMap += t._1 -> t._2; addrToIndex += t._1 -> idx; idx += 1 }
        }
      }

      val finalInsns = new Array[Instruction](idx)
      idx = 0

      def patch(i:Int): Int =
        if (addrToIndex isDefinedAt i)
          addrToIndex(i)
        else
          throw new Exception("Expected to find patch address at " + i + ", but there is none")

      for(t <- updatedInsnsMap) {
        finalInsns(idx) = t._2 match {
          case Goto(offset) => Goto(patch(t._1 + offset))
          case Goto16(offset) => Goto16(patch(t._1 + offset))
          case Goto32(offset) => Goto32(patch(t._1 + offset))
          case IfEqz(a, offset) => IfEqz(a, patch(t._1 + offset))
          case IfNez(a, offset) => IfNez(a, patch(t._1 + offset))
          case IfLtz(a, offset) => IfLtz(a, patch(t._1 + offset))
          case IfGez(a, offset) => IfGez(a, patch(t._1 + offset))
          case IfGtz(a, offset) => IfGtz(a, patch(t._1 + offset))
          case IfLez(a, offset) => IfLez(a, patch(t._1 + offset))
          case IfEq(a, b, offset) => IfEq(a, b, patch(t._1 + offset))
          case IfNe(a, b, offset) => IfNe(a, b, patch(t._1 + offset))
          case IfLt(a, b, offset) => IfLt(a, b, patch(t._1 + offset))
          case IfGe(a, b, offset) => IfGe(a, b, patch(t._1 + offset))
          case IfGt(a, b, offset) => IfGt(a, b, patch(t._1 + offset))
          case IfLe(a, b, offset) => IfLe(a, b, patch(t._1 + offset))
          case PackedSwitch(a, firstKey, targets) => PackedSwitch(a, firstKey, targets.map((offset:Int) => patch(t._1 + offset)))
          case SparseSwitch(a, keys, targets) => SparseSwitch(a, keys, targets.map((offset:Int) => patch(t._1 + offset)))
          case _ => t._2
        }
        idx += 1
      }

      val finalTries = tries.map((t:TryItem) => {
        handlers.find((ech:EncodedCatchHandler) => ech.offset == t.handlerOff) match {
          case Some(ech) => {
            val startAddr = patch(t.startAddr.safeToInt)
            new Try(startAddr, startAddr + t.insnsCount,
              ech.handlers.map((ch:CatchHandler) => {
                new CatchHandler(ch.exceptionType, patch(ch.addr.safeToInt))
              }), (if (ech.catchAllAddr == -1) -1 else patch(ech.catchAllAddr.safeToInt)))
          }
          case None => {
            println("try addr: " + t.handlerOff)
            for(ech <- handlers) println("encoded-catch-handler: " + ech.offset)
            throw new Exception("Could not find matching catch handler")
          }
        }
      })

      new CodeItem(registersSize, insSize, outsSize, finalInsns, finalTries)
    }
  }

  private def readEncodedMethods(size:Int, sourceFile:String):Array[MethodDef] = {
    val methodArray = new Array[MethodDef](size)
    var methodIdx = 0
    for(i <- 0 until size) {
      methodIdx += reader.readUleb128.safeToInt
      val accessFlags = reader.readUleb128
      val codeOff = reader.readUleb128
      val method = methods(methodIdx)
      val code = maybeReadCodeItem(codeOff, sourceFile, method.prototype.parameters, method.classType, method.name)
      methodArray(i) = new MethodDef(method, accessFlags, code)
    }
    methodArray
  }

  private def maybeReadClassData(off: Long, sourceFile: String,
                                 staticValues: Array[EncodedValue]):
    (Array[FieldDef], Array[FieldDef], Array[MethodDef], Array[MethodDef]) = {
    if (off == 0L) {
      (null, null, null, null)
    } else {
      val pos = reader.position
      reader.position(off)
      val staticFieldsSize = reader.readUleb128.safeToInt
      val instanceFieldsSize = reader.readUleb128.safeToInt
      val directMethodsSize = reader.readUleb128.safeToInt
      val virtualMethodsSize = reader.readUleb128.safeToInt
      val staticFields = readEncodedFields(staticFieldsSize, staticValues)
      val instanceFields = readEncodedFields(instanceFieldsSize, null)
      val directMethods = readEncodedMethods(directMethodsSize, sourceFile)
      val virtualMethods = readEncodedMethods(virtualMethodsSize, sourceFile)
      reader.position(pos)
      (staticFields, instanceFields, directMethods, virtualMethods)
    }
  }

  case class EncodedValueException(msg:String) extends Exception(msg)

  private def checkRangeInclusive(v:Int, min:Int, max:Int) = 
    if (v < min || v > max) throw new Exception(v + "is outside of " + min + " to " + max + " range")

  private def readEncodedInt(signed:Boolean, size:Int, numBytes:Int):Long = {
    var n = 0L
    if (!signed && size == 8) throw new OutOfRange("Unsigned Long out of Long range")
    for(i <- 0 until numBytes) n |= (reader.readUByte << i)
    n << (8 - numBytes) * 8
    if (signed) n >> (8 - numBytes) * 8 else n >>> (8 - numBytes)
  }

  def readEncodedFloat(numBytes:Int):Float = {
    // convert dex code's compressed float representation into
    // a full 4 byte float and then read it from the buffer
    val bb = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    for(i <- 0 until numBytes) bb.put(reader.readByte)
    bb.rewind
    bb.getFloat
  }

  def readEncodedDouble(numBytes:Int):Double = {
    // convert dex code's compressed double representation into
    // a full 4 byte float and then read it from the buffer
    val bb = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
    for(i <- 0 until numBytes) bb.put(reader.readByte)
    bb.rewind
    bb.getDouble
  }

  def readEncodedArray():Array[EncodedValue] = {
    val size = reader.readUleb128.safeToInt
    val vals = new Array[EncodedValue](size)
    for(i <- 0 until size) vals(i) = readEncodedValue
    vals
  }

  def readEncodedAnnotation():EncodedAnnotation = {
    val typeIdx = reader.readUleb128
    val size = reader.readUleb128.safeToInt
    val annotations = new Array[AnnotationElement](size)
    for(i <- 0 until size) {
      val nameIdx = reader.readUleb128
      val v = readEncodedValue
      annotations(i) = new AnnotationElement(strings(nameIdx.safeToInt), v)
    }
    val ea = new EncodedAnnotation(null, annotations)
    ea.javaType = lookupType(typeIdx.safeToInt, ((t: JavaType) => ea.javaType = t))
    ea
  }

  private def readEncodedValue():EncodedValue = {
    val valTypeArg = reader.readUByte
    val valArg = ((valTypeArg & 0xe0) >>> 5) + 1
    (valTypeArg & 0x1f) match {
      case VALUE_BYTE => checkRangeInclusive(valArg,1,1); new EncodedByte(reader.readByte)
      case VALUE_SHORT => checkRangeInclusive(valArg,1,2); new EncodedShort(readEncodedInt(true,2,valArg).toShort)
      case VALUE_CHAR => checkRangeInclusive(valArg,1,2); new EncodedChar(readEncodedInt(false,2,valArg).toChar)
      case VALUE_INT => checkRangeInclusive(valArg,1,4); new EncodedInt(readEncodedInt(true,4,valArg).toInt)
      case VALUE_LONG => checkRangeInclusive(valArg,1,8); new EncodedLong(readEncodedInt(true,8,valArg))
      case VALUE_FLOAT => checkRangeInclusive(valArg,1,4); new EncodedFloat(readEncodedFloat(valArg))
      case VALUE_DOUBLE => checkRangeInclusive(valArg,1,8); new EncodedDouble(readEncodedDouble(valArg))
      case VALUE_STRING => checkRangeInclusive(valArg,1,4); new EncodedString(strings(readEncodedInt(false,4,valArg).safeToInt))
      case VALUE_TYPE => {
        checkRangeInclusive(valArg,1,4)
        val et = new EncodedType(null)
        et.t = lookupType(readEncodedInt(false,4,valArg).safeToInt, ((t: JavaType) => et.t = t))
        et
      }
      case VALUE_FIELD => checkRangeInclusive(valArg,1,4); new EncodedFieldVal(fields(readEncodedInt(false,4,valArg).safeToInt))
      case VALUE_METHOD => checkRangeInclusive(valArg,1,4); new EncodedMethodVal(methods(readEncodedInt(false,4,valArg).safeToInt))
      case VALUE_ENUM => checkRangeInclusive(valArg,1,4); new EncodedEnum(fields(readEncodedInt(false,4,valArg).safeToInt))
      case VALUE_ARRAY => checkRangeInclusive(valArg,1,1); new EncodedArray(readEncodedArray)
      case VALUE_ANNOTATION => checkRangeInclusive(valArg,1,1); readEncodedAnnotation
      case VALUE_NULL => checkRangeInclusive(valArg,1,1); EncodedNull
      case VALUE_BOOLEAN => checkRangeInclusive(valArg,1,2); new EncodedBoolean(valArg == 2)
      case _ => throw new EncodedValueException("unrecognized value type " + (valTypeArg & 0x1f))
    }
  }

  private def maybeReadStaticValues(off:Long):Array[EncodedValue] = {
    if (0L == off) {
      null
    } else {
      val pos = reader.position
      reader.position(off)
      val staticValues = readEncodedArray
      reader.position(pos)
      staticValues
    }
  }

  private def readClassDefs() {
    val classDefsSize = header(ClassDefsSize.id).safeToInt
    classDefs = new Array[ClassDef](classDefsSize)
    reader.checkPosition(header(ClassDefsOffset.id))
    for(i <- 0 until classDefsSize) {
      val classIdx = reader.readUInt
      val accessFlags = reader.readUInt
      val superClassIdx = reader.readUInt
      val interfacesOff = reader.readUInt
      val sourceFileIdx = reader.readUInt
      val sourceFile = if (sourceFileIdx == NO_INDEX_LONG) null else strings(sourceFileIdx.safeToInt)
      val annotationsOff = reader.readUInt
      val classDataOff = reader.readUInt
      val staticValuesOff = reader.readUInt

      val interfaces = maybeReadTypeList(interfacesOff)
      val annotations = maybeReadAnnotationsDirectoryItem(annotationsOff)
      val staticValues = maybeReadStaticValues(staticValuesOff)
      val (staticFields, instanceFields, directMethods, virtualMethods) =
          maybeReadClassData(classDataOff, sourceFile, staticValues)
      val className = types(classIdx.safeToInt)
      val classDef = new ClassDef(className, accessFlags, null,
          interfaces, sourceFile, annotations, staticFields, instanceFields,
          directMethods, virtualMethods)
      if (superClassIdx != NO_INDEX_LONG)
        classDef.superClass = lookupType(superClassIdx.safeToInt,
            ((t: JavaType) => classDef.superClass = t))
      typeMap += className -> classDef
      classDefs(i) = classDef
    }
  }

  def readFile():Array[ClassDef] = {
    readHeader
    readStrings
    readTypes
    readPrototypes
    readFields
    readMethods
    readClassDefs
    for (t <- typeCallbacks) t._2(lookupType(t._1))
    classDefs
  }

  def getClassDefs():Array[ClassDef] = classDefs
  def getStrings():Array[String] = strings
  def getTypes():Array[String] = types
  def printPrototypes() = prototypes.foreach((x:Prototype) => println(x.shortDescriptor + " " + x.returnType + " " + (if (x.parameters == null) "none" else x.parameters.mkString(", "))))
}
