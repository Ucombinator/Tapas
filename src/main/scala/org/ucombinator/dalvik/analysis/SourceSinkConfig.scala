package org.ucombinator.dalvik.analysis

import xml.{XML, NodeSeq}

object MethodConfig {
  def DEFAULT_COST = 5
  def dexNameFromClassName(className : String) = {
    "L" + className.replace(".", "/") + ";"    
  }
  def classNameFromDexName(dexName: String) = {
    // remove L prefix and trailing ;
    dexName.substring(1, dexName.length() - 2).replace("/", ".")
  }
}

class MethodConfig(val name: String, val category: Symbol, val owner: ClassConfig) {
  override def equals(a: Any): Boolean = a match {
    case other: MethodConfig => name.equals(other.name) && category.equals(other.category)
    case _                   => false
  }
  def fullyQualifiedName = owner.name + "." + name
}

class ClassConfig(val dexName: String, val name: String,
  var methods: Set[MethodConfig] = Set.empty[MethodConfig]) {
  def addMethod(name: String, category: String): Unit =
    methods += new MethodConfig(name,Symbol(category),this)

  def methodsForCategories(cs: Set[Symbol]): Set[MethodConfig] =
    methods filter { (mc) => cs contains mc.category }
}
 
/** The config file reader for the source and sink config file used for
  * tracking methods.
  *
  * @constructor reads an XML file containing the known sources, sinks, and
  *              other interesting methods and sets up an internal config
  *              representation.
  *
  * @param fn    a string naming the XML file to read for known sources, sinks,
  *              and other interesting methods.
  */
class SourceSinkConfig(fn: String) {
  /* The following list comes from a standard list of sources and sinks.  Some
   * of these seem less like a source or sink procedure, then it does a
   * combination of source and sink methods with constructors for objects that
   * are associated with some (but not all) of them.  Also note, that this list
   * does not differentiate the method by type, only by name.  This is based on
   * the simplifying assumption that if one version of the method leaks, all of
   * the methods of this name leak. */

 
  private val xmlFile = XML.loadFile(fn)

  // Need to pull out the categories too... perhaps we need a mapping of
  // category -> class -> Set[methods]
  private def buildMap(root: NodeSeq): Map[String, ClassConfig] = {
    root.foldLeft(Map.empty[String, ClassConfig]) {
      (s, node) => (node \ "method").foldLeft(s) {
        (s, node) => {
          val className = (node \ "@class-name").text.toString()
          val dexClassName = "L" +
            className.replace(".", "/") + ";"
          val methodName = (node \ "@name").text.toString
          //println("method from xml: " + methodName)
          val category = (node \ "@category").text.toString
          if (s isDefinedAt dexClassName) {
            s(dexClassName).addMethod(methodName, category)
            s
          } else {
            val cc = new ClassConfig(dexClassName, className)
            cc.addMethod(methodName, category)
            s + (dexClassName -> cc)
          }
        }
      }
    }
  }

  private def extractCategories(m: Map[String, ClassConfig]): Map[Symbol,Set[MethodConfig]] = {
    m.foldLeft(Map.empty[Symbol,Set[MethodConfig]]) {
      (m, a) => {
        a._2.methods.foldLeft(m) {
          (m, mc) => {
            if (m isDefinedAt mc.category) {
              m + (mc.category -> (m(mc.category) + mc))
            } else {
              m + (mc.category -> Set(mc))
            }
          }
        }
      }
    }
  }

  private var _sourceMap = buildMap(xmlFile \ "sources")
  private var _sinkMap = buildMap(xmlFile \ "sinks")
  private var _otherMap = buildMap(xmlFile \ "other")
  
  // used for adding additional methods that are not sources or sinks
  def addOtherMethods(ms: List[(String,String,String)]) = {
    ms foreach {
      (m) => {
        val (className, method, cat) = m
        if (_otherMap isDefinedAt className) {
          _otherMap(className).addMethod(method, cat)
        } else {
          // adding other methods requires them to be in package format ("pkg.name"), 
          // not dex format ("Lpkg/name;")
          val cc = new ClassConfig(MethodConfig.dexNameFromClassName(className), className)
          cc.addMethod(method, cat)
          _otherMap += (className -> cc)
        }
      }
    }
  }

  def sourceMap = _sourceMap
  def sinkMap = _sinkMap
  def otherMap = _otherMap
  def generalMap = _sourceMap ++ _sinkMap ++ _otherMap
  def categoryMap = extractCategories(sourceMap) ++
                    extractCategories(sinkMap) ++
                    extractCategories(otherMap)
  def categories = categoryMap.keys
}
