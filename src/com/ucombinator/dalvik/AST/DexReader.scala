//package com.ucombinator.dalvik.DexReader

import math.min
import collection.immutable.TreeMap
import collection.mutable.{ArrayBuffer, ArrayBuilder}
import java.io.{InputStream,DataInputStream,DataOutputStream,ByteArrayOutputStream,ByteArrayInputStream,EOFException, StreamCorruptedException}
import java.nio.{ByteBuffer,ByteOrder}
import java.nio.channels.ReadableByteChannel

class DexReader(ch:ReadableByteChannel) {
  private val BUFFER_SIZE = 4096
  private val buffer = ByteBuffer.allocate(BUFFER_SIZE).order(ByteOrder.LITTLE_ENDIAN)
  private var currBuffer = buffer
  private var readingFromMainBuffer = true
  private var channelOffset = ch.read(buffer)
  private var bufferEmptyTail = buffer.remaining
  private var unknownDataBuffer = TreeMap.empty[Int,ByteBuffer]
  private var reachedEndOfFile = false

  /* initialization code */
  if (channelOffset == -1) throw new EOFException("Initialized with empty file")
  buffer.rewind

  private def checkBuffer(size:Int) {
    val remaining = currBuffer.remaining
    if (remaining < size) {
      if (readingFromMainBuffer) {
        if (reachedEndOfFile) throw new EOFException("Expected " + size + " bytes but only " + remaining + " remain in the buffer and the end of file has been reached. At position " + position)
        val remainingBytes = new Array[Byte](remaining)
        /* copy our remining bytes to the start of our buffer---this is always a relatively small number */
        buffer.get(remainingBytes)
        buffer.clear
        for (b <- remainingBytes) buffer.put(b)
        val bytesRead = ch.read(buffer)
        bufferEmptyTail = buffer.remaining
        if (bytesRead == 0 || bytesRead == -1) reachedEndOfFile = true
        if (bytesRead > 0) channelOffset += bytesRead
        buffer.rewind
      } else {
        throw new EOFException("reached end of temporary buffer")
      }
    }
  }

  private def findTmpBuffer(location:Int):(Int,ByteBuffer) = {
    val (baseAddr,buffer) = unknownDataBuffer.takeWhile((t:(Int,ByteBuffer)) => t._1 <= location).last
    buffer.position(location - baseAddr)
    (baseAddr,buffer)
  }

  private def position() = channelOffset - (currBuffer.remaining - bufferEmptyTail)

  /* constants */

  /* DEX file magic number (contains version number as well, so might need to be generalized) */
  private val DEX_FILE_MAGIC = Array(0x64, 0x65, 0x78, 0x0a, 0x30, 0x33, 0x35, 0x00)

  /* Constants indicating endianness of the file */
  private val ENDIAN_CONSTANT = 0x12345678
  private val REVERSE_ENDIAN_CONSTANT = 0x78563412

  /* Can't remember */
  private val NO_INDEX = 0xffffffff

  /* Accessibility types */
  private val ACC_PUBLIC = 0x1
  private val ACC_PRIVATE = 0x2
  private val ACC_PROTECTED = 0x4
  private val ACC_STATIC = 0x8
  private val ACC_FINAL = 0x10
  private val ACC_SYNCHRONIZED = 0x20
  private val ACC_VOLATILE = 0x40
  private val ACC_BRIDGE = 0x40
  private val ACC_TRANSIENT = 0x80
  private val ACC_VARARGS = 0x80
  private val ACC_NATIVE = 0x100
  private val ACC_INTERFACE = 0x200
  private val ACC_ABSTRACT = 0x400
  private val ACC_STRICT = 0x800
  private val ACC_SYNTHETIC = 0x1000
  private val ACC_ANNOTATION = 0x2000
  private val ACC_ENUM = 0x4000
  private val ACC_CONSTRUCTOR = 0x10000
  private val ACC_DECLARED_SYNCHRONIZED = 0x20000

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

  /* item type indicators */
  private val TYPE_HEADER_ITEM = 0x0000
  private val TYPE_STRING_ID_ITEM = 0x0001
  private val TYPE_TYPE_ID_ITEM = 0x0002
  private val TYPE_PROTO_ID_ITEM = 0x0003
  private val TYPE_FIELD_ID_ITEM = 0x0004
  private val TYPE_METHOD_ID_ITEM = 0x0005
  private val TYPE_CLASS_DEF_ITEM = 0x0006
  private val TYPE_MAP_LIST = 0x1000
  private val TYPE_TYPE_LIST = 0x1001
  private val TYPE_ANNOTATION_SET_REF_LIST = 0x1002
  private val TYPE_ANNOTATION_SET_ITEM = 0x1003
  private val TYPE_CLASS_DATA_ITEM = 0x2000
  private val TYPE_CODE_ITEM = 0x2001
  private val TYPE_STRING_DATA_ITEM = 0x2002
  private val TYPE_DEBUG_INFO_ITEM = 0x2003
  private val TYPE_ANNOTATION_ITEM = 0x2004
  private val TYPE_ENCODED_ARRAY_ITEM = 0x2005
  private val TYPE_ANNOTATIONS_DIRECTORY_ITEM = 0x2006

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

  /* visibility indicators */
  private val VISIBILITY_BUILD = 0x00
  private val VISIBILITY_RUNTIME = 0x01
  private val VISIBILITY_SYSTEM = 0x02

  private val biUshort = BigInt(0xffff)
  private val biUlong = (biUshort | biUshort << 16 | biUshort << 32 | biUshort << 48)

  /* basic readers and conversitions to support (otherwise unsupported) unsigned integer types */
  private def byteToUByte(b:Byte):Short = { val s = b.toShort; if (b < 0) { (s & 0xff).toShort } else s }
  private def shortToUShort(s:Short):Int = { val i = s.toInt; if (s < 0) i & 0xffff else i }
  private def intToUInt(i:Int):Long = { val l = i.toLong; if (i < 0) (l << 32) >>> 32 else l }
  private def longToULong(l:Long):BigInt = { val b = BigInt(l); if (l < 0) b & biUlong else b }

  private def readByte():Byte = { checkBuffer(1); currBuffer.get }
  private def readUByte():Short = byteToUByte(readByte())
  private def readShort():Short = { checkBuffer(2); currBuffer.getShort }
  private def readUShort():Int = shortToUShort(readShort())
  private def readInt():Int = { checkBuffer(4); currBuffer.getInt }
  private def readUInt():Long = intToUInt(readInt())
  private def readLong():Long = { checkBuffer(8); currBuffer.getLong }
  private def readULong():BigInt = longToULong(readLong())

  private def readUleb128():Long = {
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

  private def readUleb128p1():Long = readUleb128 - 1

  private def readSleb128():Long = {
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

  private def readMUTF8():String = {
    val size = readUleb128.safeToInt
    val strBytes = new Array[Byte](size)
    checkBuffer(size + 1)
    currBuffer.get(strBytes)
    val nullByte = currBuffer.get
    if (nullByte != 0) throw new OutOfRange("Expected null byte, but found " + nullByte)
    val ob = new ByteArrayOutputStream
    val dos = new DataOutputStream(ob)
    dos.writeChar(size.toChar)
    ob.write(strBytes,0,size)
    val dis = new DataInputStream(new ByteArrayInputStream(ob.toByteArray))
    dis.readUTF
  }

  private var checksum = 0L

  private val signature = new Array[Byte](20)
  private val header = new Array[Long](20)

  private object HeaderField extends Enumeration {
    type HeaderField = Value
    val fileSize, headerSize, endianTag, linkSize, linkOffset, mapOffset,
        stringIdsSize, stringIdsOffset, typeIdsSize, typeIdsOffset,
        protoIdsSize, protoIdsOffset, fieldIdsSize, fieldIdsOffset,
        methodIdsSize, methodIdsOffset, classDefsSize, classDefsOffset,
        dataSize, dataOffset = Value
  }

  private def readAndVerifyMagicNumber() {
    DEX_FILE_MAGIC.foreach(
      (x:Int) => {
         var ubyte = readUByte;
         if (ubyte != x)
            throw new AssertionError("start of file does not match dex file number, expected " +
                                     BigInt(x).toString(16) + " but got " + BigInt(ubyte).toString(16))
      })
  }

  private def readSignature() = { checkBuffer(20); currBuffer.get(signature) }

  def readHeader() {
    readAndVerifyMagicNumber
    checksum = readInt
    checksum = intToUInt(checksum.toInt)
    readSignature
    for(i <- 0 to 2) header(i) = readUInt
    if (header(HeaderField.endianTag.id) == REVERSE_ENDIAN_CONSTANT)
       currBuffer.order(ByteOrder.BIG_ENDIAN)
    for(i <- 3 to 19) header(i) = readUInt
  }

  class ConversionOutOfRange(msg:String) extends Exception
  class OutOfRange(msg:String) extends Exception

  class LongConverter(self:Long) {
    def safeToInt():Int =
      if (self.isValidInt)
         self.toInt
       else
         throw new ConversionOutOfRange(self + ":Long cannot be converted to an Int")
  }

  implicit def longToLongConverter(l:Long):LongConverter = new LongConverter(l)

  class Annotation(val visibility:Short, val encodedAnnotation:EncodedValue)

  type AnnotationSet = Array[Annotation]
  type AnnotationSetRefList = Array[AnnotationSet]

  class FieldAnnotation(val fieldIdx:Long, var annotationSet:AnnotationSet = null)
  class MethodAnnotation(val methodIdx:Long, var annotationSet:AnnotationSet = null)
  class ParameterAnnotation(val methodIdx:Long, var annotationSetRefList:AnnotationSetRefList = null)

  class AnnotationsDirectoryItem( 
    val fieldAnnotations:Array[FieldAnnotation],
    val methodAnnotations:Array[MethodAnnotation],
    val parameterAnnotations:Array[ParameterAnnotation],
    var classAnnotations:AnnotationSet = null)

  class EncodedField(val fieldIdxDiff:Long, val accessFlags:Long)

  class EncodedCatchHandler(val handlers:Array[(Long,Long)], val catchAllAddr:Long)

  class TryItem(val startAddr:Long, val insnCount:Int, val handlerOff:Int)

  sealed abstract class EncodedValue
  case class EncodedByte(b:Byte) extends EncodedValue
  case class EncodedShort(s:Short) extends EncodedValue
  case class EncodedChar(c:Char) extends EncodedValue
  case class EncodedInt(i:Int) extends EncodedValue
  case class EncodedLong(l:Long) extends EncodedValue
  case class EncodedFloat(f:Float) extends EncodedValue
  case class EncodedDouble(d:Double) extends EncodedValue
  case class EncodedString(idx:Long) extends EncodedValue
  case class EncodedType(idx:Long) extends EncodedValue
  case class EncodedFieldVal(idx:Long) extends EncodedValue
  case class EncodedMethodVal(idx:Long) extends EncodedValue
  case class EncodedEnum(idx:Long) extends EncodedValue
  case class EncodedArray(vals:Array[EncodedValue]) extends EncodedValue
  case class EncodedAnnotation(typeIdx:Long, elements:Array[(Long, EncodedValue)]) extends EncodedValue
  case object EncodedNull extends EncodedValue
  case class EncodedBoolean(b:Boolean) extends EncodedValue

  class CodeItem(val registerSize:Int, val insSize:Int, val outsSize:Int,
    val insns:Array[Int], val tries:Array[TryItem],
    val handlers:Array[EncodedCatchHandler], var debugInfo:DebugInfo = null)

  sealed abstract class DebugByteCode
  case object DebugEndSequence extends DebugByteCode
  case class DebugAdvancePC(addrDiff:Long) extends DebugByteCode
  case class DebugAdvanceLine(lineDiff:Long) extends DebugByteCode
  case class DebugStartLocal(registerNum:Long,nameIdx:Long,typeIdx:Long) extends DebugByteCode
  case class DebugStartLocalExtended(registerNum:Long,nameIdx:Long,typeIdx:Long,sigIdx:Long) extends DebugByteCode
  case class DebugEndLocal(registerNum:Long) extends DebugByteCode
  case class DebugRestartLocal(registerNum:Long) extends DebugByteCode
  case object DebugSetPrologueEnd extends DebugByteCode
  case object DebugSetEpilogueBegin extends DebugByteCode
  case class DebugSetFile(nameIdx:Long) extends DebugByteCode
  case class DebugSpecial(opcode:Short) extends DebugByteCode

  class DebugInfo(val lineStart:Long, val parameterNamesIdx:Array[Long],
    val debugCode:Array[DebugByteCode])

  class EncodedMethod(val methodIdxDiff:Long, val accessFlags:Long,
    var code:CodeItem = null)

  class ClassDataItem(val staticFields:Array[EncodedField],
    val instanceFields:Array[EncodedField],
    val directMethods:Array[EncodedMethod],
    val virtualMethods:Array[EncodedMethod])

  class ClassDef(val classIdx:Long, val accessFlags:Long,
    val superclassIdx:Long, val sourceFileIdx:Long,
    var interfaces:Array[Int] = null,
    var annotations:AnnotationsDirectoryItem = null,
    var classData:ClassDataItem = null, var staticValues:Array[EncodedValue] = null)

  object AnnotationType extends Enumeration {
    type AnnotationType = Value
    val ClassAnnotation, FieldAnnotation, MethodAnnotation = Value
  }
  import AnnotationType._

  sealed abstract class DataItem
  case class StringItem(idx:Long) extends DataItem
  case class ProtoItem(idx:Long, shortyIdx:Long, returnTypeIdx:Long) extends DataItem
  case class ClassDefInterfacesItem(cd:ClassDef) extends DataItem
  case class ClassDefAnnotationsItem(cd:ClassDef) extends DataItem
  case class ClassDefClassDataItem(cd:ClassDef) extends DataItem
  case class ClassDefStaticValuesItem(cd:ClassDef) extends DataItem
  case object MapList extends DataItem
  case class ClassAnnotationSetItem(ad:AnnotationsDirectoryItem) extends DataItem
  case class FieldAnnotationSetItem(fa:FieldAnnotation) extends DataItem
  case class MethodAnnotationSetItem(ma:MethodAnnotation) extends DataItem
  case class ParameterAnnotationSetItem(asrl:AnnotationSetRefList,i:Long) extends DataItem
  case class ParameterAnnotationSetRefItem(pa:ParameterAnnotation) extends DataItem
  case class AnnotationItem(as:AnnotationSet,i:Long) extends DataItem
  case class EncodedMethodCodeItem(em:EncodedMethod) extends DataItem
  case class DebugInfoItem(ci:CodeItem) extends DataItem

  private var dataMap = TreeMap.empty[Long,DataItem]
  var strings:Array[String] = null
  var types:Array[Long] = null
  var protos:Array[(Long, Long, Array[Int])] = null
  var fields:Array[(Int, Int, Long)] = null
  var methods:Array[(Int, Int, Long)] = null
  var classDefs:Array[ClassDef] = null

  def readSome(read:(Long) => Unit, pos:Long, size:Long) {
    if (position != pos) throw new StreamCorruptedException("Expected to start read at position " + 
       pos + " but, stream is at position " + position)
    for(i <- 0L until size) read(i)
  }

  def readStringId(idx:Long):Unit = dataMap += (readUInt -> new StringItem(idx))
  def readTypeId(idx:Long):Unit = types(idx.safeToInt) = readUInt
  def readProtoId(idx:Long):Unit = {
    val shortyIdx = readUInt
    val returnTypeIdx = readUInt
    val parametersOff = readUInt
    if (parametersOff == 0)
      protos(idx.safeToInt) = (shortyIdx, returnTypeIdx, new Array[Int](0))
    else 
      dataMap += parametersOff -> ProtoItem(idx, shortyIdx, returnTypeIdx)
  }
  def readFieldId(idx:Long):Unit = {
    val classIdx = readUShort
    val typeIdx = readUShort
    val nameIdx = readUInt
    fields(idx.safeToInt) = (classIdx, typeIdx, nameIdx)
  }
  def readMethodId(idx:Long):Unit = {
    val classIdx = readUShort
    val protoIdx = readUShort
    val nameIdx = readUInt
    methods(idx.safeToInt) = (classIdx, protoIdx, nameIdx)
  }
  def readClassDef(idx:Long):Unit = {
    val classIdx = readUInt
    val accessFlags = readUInt
    val superclassIdx = readUInt
    val interfacesOff = readUInt
    val sourceFileIdx = readUInt
    val annotationsOff = readUInt
    val classDataOff = readUInt
    val staticValuesOff = readUInt
    val cd = new ClassDef(classIdx, accessFlags, superclassIdx, sourceFileIdx)
    if (interfacesOff != 0) dataMap += interfacesOff -> new ClassDefInterfacesItem(cd)
    if (annotationsOff != 0) dataMap += annotationsOff -> new ClassDefAnnotationsItem(cd)
    if (classDataOff != 0) dataMap += classDataOff -> new ClassDefClassDataItem(cd)
    if (staticValuesOff != 0) dataMap += staticValuesOff -> new ClassDefStaticValuesItem(cd)
    classDefs(idx.safeToInt) = cd
  }

  def readIntoBuffer(size:Long):ByteBuffer = {
    var isize = size.safeToInt
    val tmpBuffer = ByteBuffer.allocate(isize).order(currBuffer.order)
    val bytes = new Array[Byte](BUFFER_SIZE)

    while(isize > BUFFER_SIZE) {
      checkBuffer(BUFFER_SIZE)
      currBuffer.get(bytes)
      tmpBuffer.put(bytes)
      isize -= BUFFER_SIZE
    }
    checkBuffer(isize)
    currBuffer.get(bytes,0,isize)
    tmpBuffer.put(bytes,0,isize)
    tmpBuffer.rewind
    tmpBuffer
  }

  def readString(idx:Long):Unit = strings(idx.safeToInt) = readMUTF8
  def readProtoParameters(idx:Long,shortyIdx:Long,returnTypeIdx:Long):Unit = {
    val size = readUInt
    val typeIdxes = new Array[Int](size.safeToInt)
    for(i <- 0L until size) typeIdxes(i.safeToInt) = readUShort
    protos(idx.safeToInt) = (shortyIdx, returnTypeIdx, typeIdxes)
  }
  def readInterfaces(cd:ClassDef) {
    val size = readUInt
    val typeIdxes = new Array[Int](size.safeToInt)
    for(i <- 0L until size) typeIdxes(i.safeToInt) = readUShort
    cd.interfaces = typeIdxes
  }
  def readAnnotationsDirectory(cd:ClassDef) {
    val classAnnotationOff = readUInt
    val fieldsSize = readUInt
    val annotatedMethodsSize = readUInt
    val annotatedParametersSize = readUInt
    val fields = new Array[FieldAnnotation](fieldsSize.safeToInt)
    val methods = new Array[MethodAnnotation](annotatedMethodsSize.safeToInt)
    val parameters = new Array[ParameterAnnotation](annotatedParametersSize.safeToInt)
    val ad = new AnnotationsDirectoryItem(fields, methods, parameters)
    if (classAnnotationOff != 0) dataMap += classAnnotationOff -> ClassAnnotationSetItem(ad)
    for(i <- 0L until fieldsSize) {
      val fieldIdx = readUInt
      val fa = new FieldAnnotation(fieldIdx)
      fields(i.safeToInt) = fa
      dataMap += readUInt -> FieldAnnotationSetItem(fa)
    }
    for(i <- 0L until annotatedMethodsSize) {
      val methodIdx = readUInt
      val ma = new MethodAnnotation(methodIdx)
      methods(i.safeToInt) = ma
      dataMap += readUInt -> MethodAnnotationSetItem(ma)
    }
    for(i <- 0L until annotatedParametersSize) {
      val methodIdx = readUInt
      val pa = new ParameterAnnotation(methodIdx)
      dataMap += readUInt -> ParameterAnnotationSetRefItem(pa)
    }
    cd.annotations = ad
  }
  def readEncodedFields(size:Int):Array[EncodedField] = {
    val fields = new Array[EncodedField](size)
    for(i <- 0 until size) {
      val fieldIdxDiff = readUleb128
      val accessFlags = readUleb128
      fields(i) = new EncodedField(fieldIdxDiff, accessFlags)
    }
    fields
  }
  def readEncodedMethods(size:Int):Array[EncodedMethod] = {
    val methods = new Array[EncodedMethod](size)
    for(i <- 0 until size) {
      val methodIdxDiff = readUleb128
      val accessFlags = readUleb128
      val codeOff = readUleb128
      val em = new EncodedMethod(methodIdxDiff, accessFlags)
      methods(i) = em
      if (codeOff != 0) dataMap += codeOff -> EncodedMethodCodeItem(em)
    }
    methods
  }
  def readClassData(cd:ClassDef) {
    val staticFieldsSize = readUleb128
    val instanceFieldsSize = readUleb128
    val directMethodsSize = readUleb128
    val virtualMethodsSize = readUleb128
    val staticFields = readEncodedFields(staticFieldsSize.safeToInt)
    val instanceFields = readEncodedFields(instanceFieldsSize.safeToInt)
    val directMethods = readEncodedMethods(directMethodsSize.safeToInt)
    val virtualMethods = readEncodedMethods(virtualMethodsSize.safeToInt)
    cd.classData = new ClassDataItem(staticFields, instanceFields, directMethods, virtualMethods)
  }

  def checkRangeInclusive(v:Int, min:Int, max:Int) = 
    if (v < min || v > max) throw new Exception(v + "is outside of " + min + " to " + max + " range")

  def readEncodedInt(signed:Boolean,size:Int,numBytes:Int):Long = {
    var n = 0L
    if (!signed && size == 8) throw new Exception("Unsigned Long out of Long range")
    for(i <- 0 until numBytes) n |= (readUByte << i)
    n << (8 - numBytes) * 8
    if (signed) n >> (8 - numBytes) * 8 else n >>> (8 - numBytes)
  }

  def readEncodedFloat(numBytes:Int):Float = {
    val bb = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    for(i <- 0 until numBytes) bb.put(readByte)
    bb.rewind
    bb.getFloat
  }

  def readEncodedDouble(numBytes:Int):Double = {
    val bb = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
    for(i <- 0 until numBytes) bb.put(readByte)
    bb.rewind
    bb.getDouble
  }

  def readEncodedArray():Array[EncodedValue] = {
    val size = readUleb128
    val vals = new Array[EncodedValue](size.safeToInt)
    for(i <- 0 until size.safeToInt) vals(i) = readEncodedValue
    vals
  }

  def readEncodedAnnotation():EncodedAnnotation = {
    val typeIdx = readUleb128
    val size = readUleb128
    val annotations = new Array[(Long,EncodedValue)](size.safeToInt)
    for(i <- 0 until size.safeToInt) {
      val nameIdx = readUleb128
      val v = readEncodedValue
      annotations(i) = (nameIdx, v)
    }
    new EncodedAnnotation(typeIdx, annotations)
  }

  class EncodedValueException(msg:String) extends Exception

  def readEncodedValue():EncodedValue = {
    val valTypeArg = readUByte
    val valArg = ((valTypeArg & 0xe0) >>> 5) + 1
    (valTypeArg & 0x1f) match {
      case VALUE_BYTE => checkRangeInclusive(valArg,1,1); new EncodedByte(readByte)
      case VALUE_SHORT => checkRangeInclusive(valArg,1,2); new EncodedShort(readEncodedInt(true,2,valArg).toShort)
      case VALUE_CHAR => checkRangeInclusive(valArg,1,2); new EncodedChar(readEncodedInt(false,2,valArg).toChar)
      case VALUE_INT => checkRangeInclusive(valArg,1,4); new EncodedInt(readEncodedInt(true,4,valArg).toInt)
      case VALUE_LONG => checkRangeInclusive(valArg,1,8); new EncodedLong(readEncodedInt(true,8,valArg))
      case VALUE_FLOAT => checkRangeInclusive(valArg,1,4); new EncodedFloat(readEncodedFloat(valArg))
      case VALUE_DOUBLE => checkRangeInclusive(valArg,1,8); new EncodedDouble(readEncodedDouble(valArg))
      case VALUE_STRING => checkRangeInclusive(valArg,1,4); new EncodedString(readEncodedInt(false,4,valArg))
      case VALUE_TYPE => checkRangeInclusive(valArg,1,4); new EncodedType(readEncodedInt(false,4,valArg))
      case VALUE_FIELD => checkRangeInclusive(valArg,1,4); new EncodedFieldVal(readEncodedInt(false,4,valArg))
      case VALUE_METHOD => checkRangeInclusive(valArg,1,4); new EncodedMethodVal(readEncodedInt(false,4,valArg))
      case VALUE_ENUM => checkRangeInclusive(valArg,1,4); new EncodedEnum(readEncodedInt(false,4,valArg))
      case VALUE_ARRAY => checkRangeInclusive(valArg,1,1); new EncodedArray(readEncodedArray)
      case VALUE_ANNOTATION => checkRangeInclusive(valArg,1,1); readEncodedAnnotation
      case VALUE_NULL => checkRangeInclusive(valArg,1,1); EncodedNull
      case VALUE_BOOLEAN => checkRangeInclusive(valArg,1,2); new EncodedBoolean(valArg == 2)
      case _ => throw new EncodedValueException("unrecognized value type " + (valTypeArg & 0x1f))
    }
  }

  def readStaticValues(cd:ClassDef) {
    cd.staticValues = readEncodedArray
  }

  def readMapList() {
    val size = readUInt
    val mapItems = new Array[(Int,Long,Long)](size.safeToInt)
    for(i <- 0L until size) {
      val itemType = readUShort
      val unused = readUShort
      val size = readUInt
      val offset = readUInt
      mapItems(i.safeToInt) = (itemType, size, offset)
    }
  }

  def readAnnotationSet():AnnotationSet = {
    val size = readUInt
    val as = new AnnotationSet(size.safeToInt)
    for(i <- 0L until size) dataMap += readUInt -> AnnotationItem(as,i)
    as
  }

  def readClassAnnotation(ad:AnnotationsDirectoryItem) {
    ad.classAnnotations = readAnnotationSet
  }
 
  def readFieldAnnotation(fa:FieldAnnotation) {
    fa.annotationSet = readAnnotationSet
  }

  def readMethodAnnotation(ma:MethodAnnotation) {
    ma.annotationSet = readAnnotationSet
  }

  def readParameterAnnotation(asrl:AnnotationSetRefList, i:Long) {
    asrl(i.safeToInt) = readAnnotationSet
  }

  def readParameterAnnotationSet(pa:ParameterAnnotation) {
    val size = readUInt
    val asrl = new AnnotationSetRefList(size.safeToInt)
    for(i <- 0L until size) dataMap += readUInt -> ParameterAnnotationSetItem(asrl,i)
  }

  def readAnnotation(as:AnnotationSet, i:Long) {
    val visibility = readUByte
    val annotation = readEncodedAnnotation
    as(i.safeToInt) = new Annotation(visibility, annotation)
  }

  def readCodeItem(em:EncodedMethod) {
    val registersSize = readUShort
    val insSize = readUShort
    val outsSize = readUShort
    val triesSize = readUShort
    val debugInfoOff = readUInt
    val insnsSize = readUInt
    val insns = new Array[Int](insnsSize.safeToInt)
    for (i <- 0L until insnsSize) insns(i.safeToInt) = readUShort
    if ((insnsSize % 2) == 1 && triesSize > 0) readUShort
    val tries = new Array[TryItem](triesSize)
    for (i <- 0 until triesSize) tries(i) = readTryItem
    val handlers = if (triesSize > 0) readEncodedCatchHandlerList else null
    em.code = new CodeItem(registersSize, insSize, outsSize, insns, tries, handlers)
    if (debugInfoOff != 0) dataMap += debugInfoOff -> DebugInfoItem(em.code)
  }

  def readTryItem():TryItem = {
    val startAddr = readUInt
    val insnCount = readUShort
    val handlerOff = readUShort
    new TryItem(startAddr, insnCount, handlerOff)
  }

  def readEncodedCatchHandlerList() = {
    val size = readUleb128
    val list = new Array[EncodedCatchHandler](size.safeToInt)
    for(i <- 0L until size) list(i.safeToInt) = readEncodedCatchHandler
    list
  }

  def readEncodedCatchHandler():EncodedCatchHandler = {
    val size = readSleb128
    val isize = size.abs.safeToInt
    val handlers = new Array[(Long,Long)](isize)
    for(i <- 0 until isize) {
      val typeIdx = readUleb128
      val addr = readUleb128
      handlers(i) = (typeIdx, addr)
    }
    new EncodedCatchHandler(handlers, if (size < 0) readUleb128 else -1)
  }

  class UnrecognizedDebugCodeItemException(msg:String) extends Exception

  def readDebugInfo(ci:CodeItem) {
    val lineStart = readUleb128
    val parametersSize = readUleb128
    val parameterNames = new Array[Long](parametersSize.safeToInt)
    val debugCode = ArrayBuilder.make[DebugByteCode]
    for (i <- 0L until parametersSize) parameterNames(i.safeToInt) = readUleb128p1
    var lastCodeItem = readUByte
    

    while(lastCodeItem != DBG_END_SEQUENCE) {
      debugCode += (lastCodeItem match {
        case DBG_ADVANCE_PC => DebugAdvancePC(readUleb128)
        case DBG_ADVANCE_LINE => DebugAdvanceLine(readSleb128)
        case DBG_START_LOCAL => {
          val registerNum = readUleb128
          val nameIdx = readUleb128p1
          val typeIdx = readUleb128p1
          DebugStartLocal(registerNum,nameIdx,typeIdx)
        }
        case DBG_START_LOCAL_EXTENDED => {
          val registerNum = readUleb128
          val nameIdx = readUleb128p1
          val typeIdx = readUleb128p1
          val sigIdx = readUleb128p1
          DebugStartLocalExtended(registerNum,nameIdx,typeIdx,sigIdx)
        }
        case DBG_END_LOCAL => DebugEndLocal(readUleb128)
        case DBG_RESTART_LOCAL => DebugRestartLocal(readUleb128)
        case DBG_SET_PROLOGUE_END => DebugSetPrologueEnd
        case DBG_SET_EPILOGUE_BEGIN => DebugSetEpilogueBegin
        case DBG_SET_FILE => DebugSetFile(readUleb128p1)
        case _ => {
          if (lastCodeItem >= 0x0a && lastCodeItem <= 0xff)
            DebugSpecial(lastCodeItem)
          else
            throw new UnrecognizedDebugCodeItemException(lastCodeItem + " is not a recognized debugging bytecode")
        }
      })
      lastCodeItem = readUByte
    }
    ci.debugInfo = new DebugInfo(lineStart,parameterNames,debugCode.result)
  }

  def readData() {
    val readPositions = new ArrayBuffer[Long]
    while(!dataMap.isEmpty) {
      val (nextPosition,elem) = dataMap.head
      dataMap = dataMap.tail
      val tmpChannelOffset = channelOffset
      val tmpBufferEmptyTail = bufferEmptyTail
      if (nextPosition > position)
        unknownDataBuffer += (position -> readIntoBuffer(nextPosition-position))
      if (nextPosition < position) {
        val (offset, tmpBuffer) = findTmpBuffer(nextPosition.safeToInt)
        readingFromMainBuffer = false
        channelOffset = offset + tmpBuffer.capacity
        currBuffer = tmpBuffer
        bufferEmptyTail = 0
      } 

      elem match {
        case StringItem(idx) => readString(idx)
        case ProtoItem(idx,shortyIdx,returnTypeIdx) => readProtoParameters(idx,shortyIdx,returnTypeIdx)
        case ClassDefInterfacesItem(cd) => readInterfaces(cd)
        case ClassDefAnnotationsItem(cd) => readAnnotationsDirectory(cd)
        case ClassDefClassDataItem(cd) => readClassData(cd)
        case ClassDefStaticValuesItem(cd) => readStaticValues(cd)
        case MapList => readMapList
        case ClassAnnotationSetItem(ad) => readClassAnnotation(ad)
        case FieldAnnotationSetItem(fa) => readFieldAnnotation(fa)
        case MethodAnnotationSetItem(ma) => readMethodAnnotation(ma)
        case ParameterAnnotationSetItem(asrl,i) => readParameterAnnotation(asrl,i)
        case ParameterAnnotationSetRefItem(pa) => readParameterAnnotationSet(pa)
        case AnnotationItem(as,i) => readAnnotation(as, i)
        case EncodedMethodCodeItem(em) => readCodeItem(em)
        case DebugInfoItem(ci) => readDebugInfo(ci)
      }

      if (!readingFromMainBuffer) {
        channelOffset = tmpChannelOffset
        currBuffer = buffer
        bufferEmptyTail = tmpBufferEmptyTail
        readingFromMainBuffer = true
      }
    }
  }

  def readFile() {
    readHeader
    dataMap += header(HeaderField.mapOffset.id) -> MapList
    strings = new Array[String](header(HeaderField.stringIdsSize.id).safeToInt)
    types = new Array[Long](header(HeaderField.typeIdsSize.id).safeToInt)
    protos = new Array[(Long,Long,Array[Int])](header(HeaderField.protoIdsSize.id).safeToInt)
    fields = new Array[(Int,Int,Long)](header(HeaderField.fieldIdsSize.id).safeToInt)
    methods = new Array[(Int,Int,Long)](header(HeaderField.methodIdsSize.id).safeToInt)
    classDefs = new Array[ClassDef](header(HeaderField.classDefsSize.id).safeToInt)
    readSome(readStringId, header(HeaderField.stringIdsOffset.id), header(HeaderField.stringIdsSize.id))
    readSome(readTypeId, header(HeaderField.typeIdsOffset.id), header(HeaderField.typeIdsSize.id))
    readSome(readProtoId, header(HeaderField.protoIdsOffset.id), header(HeaderField.protoIdsSize.id))
    readSome(readFieldId, header(HeaderField.fieldIdsOffset.id), header(HeaderField.fieldIdsSize.id))
    readSome(readMethodId, header(HeaderField.methodIdsOffset.id), header(HeaderField.methodIdsSize.id))
    readSome(readClassDef, header(HeaderField.classDefsOffset.id), header(HeaderField.classDefsSize.id))
    readData
  }

  def printParts(printAll:Boolean = false) {
    println("checksum: " + checksum)
    println("signature: " + signature.mkString(", "))
    println("header: " + header.mkString(", "))
    if (printAll) println("dataMap: " + dataMap.mkString(", "))
  }
}

