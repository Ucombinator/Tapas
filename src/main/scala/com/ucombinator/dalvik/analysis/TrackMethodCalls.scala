package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST._

/** Class for performing abstract evaluation on a single method.
  *
  * @constructor creates a new method abstract evaluator for the specified
  *              method of the specified class.
  * @param clazz the class containing the method.
  * @param method the method defintion to be analyzed.
  */
class MethodAbstractEval(clazz: ClassDef, method: MethodDef) {
  
  /** A simple method to process the bytecode of a method looking for the set
    * of methods that can be called from this method.
    *
    * This procedure does not perform any abstract evaluation, it is here
    * mostly to serve as a first pass approximation of what we want this
    * analzyer to do.
    *
    * @return returns the set of method definitions that could be called here.
    */
  def gatherCalledMethods: Set[Method] = {
    if (method.code == null || method.code.insns == null) {
      Set.empty[Method]
    } else {
      method.code.insns.foldLeft(Set.empty[Method]) {
        (set, insn) => insn match {
          case InvokeSuper(args, b)          => set + b
          case InvokeDirect(args, b)         => set + b
          case InvokeStatic(args, b)         => set + b
          case InvokeInterface(args, b)      => set + b
          case InvokeVirtual(args, b)        => set + b
          case InvokeVirtualRange(c, a, b)   => set + b
          case InvokeSuperRange(c, a, b)     => set + b
          case InvokeDirectRange(c, a, b)    => set + b
          case InvokeStaticRange(c, a, b)    => set + b
          case InvokeInterfaceRange(c, a, b) => set + b
          case _                             => set
        }
      }
    }
  }
}

class SourceSinkMethodCallAnalyzer(clazzes: Array[ClassDef]) {
  /* The following list comes from a standard list of sources and sinks.  Some
   * of these seem less like a source or sink procedure, then it does a
   * combination of source and sink methods with constructors for objects that
   * are associated with some (but not all) of them.  Also note, that this list
   * does not differentiate the method by type, only by name.  This is based on
   * the simplifying assumption that if one version of the method leaks, all of
   * the methods of this name leak. */
  val knownSources = Map(
        "Landroid/accounts/AccountManager;" -> Array("getAccounts", "getAccountsByType", "getAccountsByTypeAndFeatures", "getAuthTokenByFeatures", "hasFeatures", "addOnAccountsUpdatedListener"),
        "Landroid/app/ActivityManager;" -> Array("getRecentTasks", "getRunningTasks"),
        "Landroid/content/ContentResolver;" -> Array("query", "openFileDescriptor", "openInputStream", "openOutputStream"),
        "Landroid/content/Intent;" -> Array("getExtras"),
        "Landroid/hardware/Camera;" -> Array("open"),
        "Landroid/hardware/SensorEventListener;" -> Array("onAccuracyChanged", "onSensorChanged"),
        "Landroid/hardware/SensorListener;" -> Array("onAccuracyChanged", "onSensorChanged"),
        "Landroid/location/Location;" -> Array("getLatitude", "getLongitude"),
        "Landroid/location/LocationListener;" -> Array("onLocationChanged"),
        "Landroid/location/LocationManager;" -> Array("getLastKnownLocation", "getProvider", "isProviderEnabled", "getBestProvider", "getProviders", "requestLocationUpdates"),
        "Landroid/media/AudioRecord;" -> Array("AudioRecord"),
        "Landroid/media/MediaRecorder;" -> Array("setAudioSource", "setVideoSource"),
        "Landroid/os/DropBoxManager;" -> Array("getNextEntry"),
        "Landroid/speech/SpeechRecognizer;" -> Array("cancel", "startListening", "stopListening"),
        "Landroid/telephony/cdma/CdmaCellLocation;" -> Array("getBaseStationId"),
        "Landroid/telephony/TelephonyManager;" -> Array("getCellLocation", "getDeviceId", "getDeviceSoftwareVersion", "getLine1Number", "getNetworkCountryIso", "getSimSerialNumber", "getSubscriberId", "getVoiceMailNumber", "getVoiceMailAlphaTag", "getNeighboringCellInfo", "incomingCallNumber"), 
        "Landroid/view/OrientationListener;" -> Array("onAccuracyChanged", "onSensorChanged"),
        "Landroid/widget/QuickContactBadge;" -> Array("assignContactFromEmail", "assignContactFromPhone"),
        "Ljava/io/FileInputStream;" -> Array("FileInputStream", "available", "read", "getFD", "getChannel", "skip", "close", "finalize"),
        "Landroid/telephony/SmsMessage;" -> Array("getMessageBody", "getOriginatingAddress"),
        "Landroid/location/Location;" -> Array("getLatitude", "getLongitude"),
        "Landroid/telephony/TelephonyManager;" -> Array("getDeviceId"),
        "Ljava/io/File;" -> Array("listFiles"),
        "Landroid/database/Cursor;" -> Array("getString")
     )
  val knownSinks = Map(
        "Landroid/bluetooth/BluetoothSocket;" -> Array("getOutputStream"),
        "Landroid/content/Intent;" -> Array("setData", "Intent"),
        "Landroid/net/http/AndroidHttpClient;" -> Array("execute"),
        "Landroid/provider/Browser;" -> Array("addSearchUrl", "saveBookmark", "sendString"),
        "Landroid/telephony/gsm/SmsManager;" -> Array("sendDataMessage", "sendMultipartTextMessage", "sendTextMessage"),
        "Landroid/telephony/SmsManager;" -> Array("SmsManager", "sendDataMessage", "sendMultipartTextMessage", "sendTextMessage", "getSubmitPdu"),
        "Landroid/util/Log;" -> Array("isLoggable", "d", "e", "i", "println", "v", "w", "wtf", "getStackTraceString", "Log"),
        "Landroid/webkit/WebSettings;" -> Array("setBlockNetworkLoads"),
        "Landroid/webkit/WebView;" -> Array("WebView"),
        "Ljava/io/FileOutputStream;" -> Array("FileOutputStream", "getFD", "getChannel", "close", "finalize", "write"),
        "Ljava/io/FileWriter;" -> Array("FileWriter"),
        "Ljava/io/RandomAccessFile;" -> Array("write", "writeByte", "writeBytes", "writeChar", "writeChars"),
        "Ljava/net/DatagramSocket;" -> Array("DatagramSocket"),
        "Ljava/net/HttpURLConnection;" -> Array("HttpURLConnection"),
        "Ljava/net/MulticastSocket;" -> Array("MulticastSocket"),
        "Ljava/net/NetworkInterface;" -> Array("NetworkInterface"),
        "Ljava/net/ServerSocket;" -> Array("ServerSocket", "bind"),
        "Ljava/net/Socket;" -> Array("Socket"),
        "Ljava/net/URLConnection;" -> Array("getInputStream", "connect"),
        "Lorg/apache/commons/logging/Log;" -> Array("isDebugEnabled", "isErrorEnabled", "isFatalEnabled", "isInfoEnabled", "isTraceEnabled", "isWarnEnabled", "debug", "error", "fatal", "info", "trace", "warn"),
        "Lorg/apache/http/client/HttpClient;" -> Array("execute"),
        "Lorg/apache/http/impl/client/AbstractHttpClient;" -> Array("execute"),
        "Lorg/apache/http/impl/client/DefaultHttpClient;" -> Array("DefaultHttpClient"),
        "Ljava/net/URL;" -> Array("openConnection"),
        "Ljava/io/DataOutputStream;" -> Array("writeBytes", "write"),
        "Ljava/io/FileOutputStream;" -> Array("write")
     )
  var _sources = Set.empty[MethodDef]
  var _sinks   = Set.empty[MethodDef]
  var classMap = clazzes.foldLeft(Map.empty[String,ClassDef]) { (m, cd) => m + (cd.name -> cd) }

  for (cd <- clazzes) {
    for (md <- cd.methods) {
      new MethodAbstractEval(cd, md).gatherCalledMethods foreach {
        (m) => {
           if (knownSources isDefinedAt m.className) {
             val knownSourceMethods = knownSources(m.className)
             if (knownSourceMethods contains m.name)
               _sources += md
           }
           if (knownSinks isDefinedAt m.className) {
             val knownSinkMethods = knownSinks(m.className)
             if (knownSinkMethods contains m.name)
               _sinks += md
           }
        }
      }
    }
  }

  def sources = _sources
  def sinks   = _sinks
}

class MethodCallAnalyzer(clazzes: Array[ClassDef]) {
  var classMap = Map.empty[String,Map[String,Option[Set[Method]]]]

  private def doAnalysis(className: String, methodName: String): Option[Set[Method]] = {
    clazzes find { (cd) => cd.name == className } match {
      case Some(classDef) =>
        classDef.methods find { (md) => md.name == methodName } match {
          case Some(methodDef) => {
            val evaluator = new MethodAbstractEval(classDef, methodDef)
            Some(evaluator.gatherCalledMethods)
          }
          case None => None
        }
      case None => None
    }
  }
  
  def lookupMethods(className: String, methodName: String): Option[Set[Method]] = {
    if (classMap isDefinedAt className) {
      val methodMap = classMap(className)
      if (methodMap isDefinedAt methodName) {
        methodMap(methodName)
      } else {
        val methods = doAnalysis(className, methodName)
        classMap = classMap.updated(className, classMap(className) + (methodName -> methods))
        methods
      }
    } else {
      val methods = doAnalysis(className, methodName)
      classMap += className -> Map(methodName -> methods)
      methods
    }
  }
}
