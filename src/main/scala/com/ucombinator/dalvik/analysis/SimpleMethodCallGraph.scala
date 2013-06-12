package com.ucombinator.dalvik.analysis

import com.ucombinator.dalvik.AST._

/** Proxy class to wrap ClassDef that stores links to subclasses and a map of
  * method names to MethodDefProxy objects.  It may make sense to integrate
  * this information into the ClassDef class to avoid the need for the proxy.
  *
  * @constructor creates a new ClassDef proxy
  *
  * @param clazz     the ClassDef object being proxied (or null if there is
  *                  only an AbstractType representing the class)
  * @param methodMap mapping from class defs to MethodDefProxies
  */
class ClassDefProxy(val clazz: ClassDef, var methodMap: Map[String, MethodDefProxy]) {
  private var _subclasses = Set.empty[ClassDefProxy]

  /** Setter method for the subclasses field.  Subclasses are stored in a set
    * and may include both classes that inherit from this class or classes that
    * implement this classes interface (because classes and interfaces are not
    * distinguished the dalvik DEX file).
    *
    * this procedure is usually called used as: 
    * {{{
    * classDefProxyObj.subclasses += classDefProxySubClass
    * }}}
    *
    * @param sc the update ClassDefProxy set
    * @return Unit
    */
  def subclasses_=(sc : Set[ClassDefProxy]) : Unit = _subclasses = sc

  /** Accessor method that returns the set of subclass ClassDefProxies.
    *
    * @return subclasses of the current class
    */
  def subclasses = _subclasses
}

/** Proxy class to wrap a MethodDef or Method object that stores both a set of
  * methods this method may call when it is invoked and a set of methods that
  * this method may be called by.  It may make sense to integrate these fields
  * into the MethodDef or Method class.
  *
  * Note: When the MethodDefProxy wraps a Method, it is because the class
  * containing the method does not have a ClassDef, but is instead represented
  * as an AbstractType.
  *
  * @constructor builds a new MethodDefProxy wrapping a MethodDef (if the class
  *              definition is in the DEX file) or a Method (if the class is
  *              left abstract, or corresponds to a built-in library)
  *
  * @param methodDef the MethodDef object being wrapped by this proxy (or null)
  * @param method    the Method object being wrapped by this proxy (when the
  *                  MethodDef is not available)
  */
class MethodDefProxy(val methodDef: MethodDef, val method: Method) {
  /** Constructor for when the MethodDef is available
    * @param methodDef the MethodDef object being wrapped by this proxy
    */
  def this(methodDef: MethodDef) = this(methodDef, null)
  /** Constructor for when the MethodDef is available
    * @param method the Method object being wrapped by this proxy
    */
  def this(method: Method) = this(null, method)

  private var _calledBy = Set.empty[MethodDefProxy]
  private var _calls    = Set.empty[MethodDefProxy]

  /** Setter method for the calls field.  The MethodDefProxy for the called
    * method is stored in a set (so there are no duplicates).
    *
    * This is generally used as:
    * {{{
    * methodDefProxyObject.calls += calledMethodDefProxyObject
    * }}}
    * 
    * @param callsSet the full set of methods called by this method.
    * @return Unit
    */
  def calls_=(callsSet: Set[MethodDefProxy]) : Unit = _calls = callsSet

  /** Accessor method for the calls field.
    * @returns a set of MethodDefProxy objects that are called by this method
    */
  def calls = _calls

  /** Setter method for the calledBy field.  The MethodDefProxy for methods
    * that call this method stored as a set (so there are no duplicates).
    *
    * This is generally used as:
    * {{{
    * methodDefProxyObject.calledBy += calledMethodDefProxyObject
    * }}}
    *
    * @param calledBySet the full set of methods that call this method.
    * @return Unit
    */
  def calledBy_=(calledBySet: Set[MethodDefProxy]) : Unit = _calledBy = calledBySet

  /** Accessor method for the calledBy field.
    * @returns a set of MethodDefProxy objects that call this method
    */
  def calledBy = _calledBy
}

/** Class to build the method call graph by looking for call sites and
  * deteremining which method is called in each location.  When a method is
  * found, the specific class and method are added to the called list along
  * with any method that might override this method in a subclass of this
  * class.
  * 
  * This class assumes it is being handed all of the ClassDefs for the program
  * being analyzed.
  *
  * @constructor builds a map from class names to the ClassDefProxy objects and
  * populates the subclasses field of the ClassDefProxy, and the calls and
  * calledBy Fields of each MethodDefProxy.
  *
  * @params classes an array of all the class definitions in this android
  * program.
  */
class SimpleMethodCallGraph(classes: Array[ClassDef]) {
  private def javaTypeToName(jt: JavaType) : String = {
    jt match {
      case cd: ClassDef     => cd.name
      case at: AbstractType => at.nameOf
      case _                => println("Unable to turn " + jt + " into a name") ; null
    }
  }

  private var _classMap = classes.foldLeft(Map.empty[String,ClassDefProxy]) {
    (map, clazz) =>
      map + (clazz.name ->
             new ClassDefProxy(clazz,
               clazz.methods.foldLeft(Map.empty[String,MethodDefProxy]) {
                 (map, method) => map + (method.name -> new MethodDefProxy(method))
               }))
    }
  
  protected def addMethod(methodProxy : MethodDefProxy)(calledMethod: Method) {
    def addToClass(clazzProxy: ClassDefProxy) : Unit = {
      val mdProxy = if (clazzProxy.methodMap isDefinedAt calledMethod.name) {
        clazzProxy.methodMap(calledMethod.name)
      } else {
        val mdProxy = new MethodDefProxy(calledMethod)
        clazzProxy.methodMap += calledMethod.name -> mdProxy
        mdProxy
      }
      methodProxy.calls += mdProxy
      mdProxy.calledBy += methodProxy
      clazzProxy.subclasses foreach addToClass
    }
    val className = javaTypeToName(calledMethod.classType)
    val classProxy = if (_classMap isDefinedAt className) {
      _classMap(className)
    } else {
      val classProxy = new ClassDefProxy(null,
                                         Map.empty[String, MethodDefProxy])
      _classMap += className -> classProxy
      classProxy
    }
    addToClass(classProxy)
  }
  
  protected def processInstructions(methodProxy: MethodDefProxy)(method: MethodDef) {
    val code = method.code
    val add = addMethod(methodProxy)_
    code.insns foreach {
      (insn) => insn match {
        case InvokeSuper(args, b)          => add(b)
        case InvokeDirect(args, b)         => add(b)
        case InvokeStatic(args, b)         => add(b)
        case InvokeInterface(args, b)      => add(b)
        case InvokeVirtual(args, b)        => add(b)
        case InvokeVirtualRange(c, a, b)   => add(b)
        case InvokeSuperRange(c, a, b)     => add(b)
        case InvokeDirectRange(c, a, b)    => add(b)
        case InvokeStaticRange(c, a, b)    => add(b)
        case InvokeInterfaceRange(c, a, b) => add(b)
        case _                             => false
      }
    }
  }

  _classMap foreach {
    (t) => {
      val clazz = t._2
      def addSubclassToClass(superClass: JavaType) : Unit = {
        superClass match {
          case cd: ClassDef     => _classMap(cd.name).subclasses += clazz
          case at: AbstractType =>
            if (_classMap isDefinedAt at.nameOf) {
              _classMap(at.nameOf).subclasses += clazz
            } else {
              val classDef = new ClassDefProxy(null, Map.empty[String, MethodDefProxy])
              classDef.subclasses += clazz
              _classMap += (at.nameOf -> classDef)
            }
          case _                => println("Unexpected type: " + superClass) 
        }
      }
      addSubclassToClass(clazz.clazz.superClass)
      if (clazz.clazz.interfaces != null) clazz.clazz.interfaces foreach addSubclassToClass
    }
  }

  classes foreach {
    (clazz) => {
      val classProxy = _classMap(clazz.name)
      clazz.methods foreach {
        (method) => {
          val methodProxy: MethodDefProxy = classProxy.methodMap(method.name)
          val process = processInstructions(methodProxy)_
          if (method.code != null && method.code.insns != null) {
        	process(method)
          }
        }
      }
    }
  }

  /** Accessor for getting at the mapping from name to ClassDefProxy objects.
    *
    * @returns map from name to ClassDefProxy object for every class,
    *          interface, and base library used in this application.
    */
  def classMap = _classMap
}
