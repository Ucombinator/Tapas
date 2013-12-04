package org.ucombinator.dalvik.analysis

import xml.{XML, NodeSeq}

class MethodConfig(val name: String, val category: Symbol, val owner: ClassConfig) {
  override def equals(a: Any): Boolean = a match {
    case other: MethodConfig => name.equals(other.name) && category.equals(other.category)
    case _                   => false
  }
}

class ClassConfig(val name: String,
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
          val className = "L" +
            (node \ "@class-name").toString.replace(".", "/") + ";"
          val methodName = (node \ "@name").toString
          val category = (node \ "@category").toString
          if (s isDefinedAt className) {
            s(className).addMethod(methodName, category)
            s
          } else {
            val cc = new ClassConfig(className)
            cc.addMethod(methodName, category)
            s + (className -> cc)
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

  def addMethods(ms: List[(String,String,String)]) = {
    ms foreach {
      (m) => {
        val className = m._1
        if (_otherMap isDefinedAt m._1) {
          _otherMap(m._1).addMethod(m._2, m._3)
        } else {
          val cc = new ClassConfig(m._1)
          cc.addMethod(m._2, m._3)
          _otherMap += (m._1 -> cc)
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
