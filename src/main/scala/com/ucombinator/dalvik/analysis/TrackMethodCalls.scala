package com.ucombinator.dalvik.analysis

import xml.{XML, NodeSeq}
import com.ucombinator.dalvik.AST._

/** A simple analyzer that builds sets of methods that are called by one of the
  * source, sink, or other intersting methods.  This class makes use of the
  * SimpleMethodCallGraph to build the initial call graph and then retrieves
  * known source, sink, and other functions to find where they are called.
  *
  * The idea is to use this as the basis for running a reverse abstract
  * interpretation through a relational program.
  *
  * @constructor reads an XML file containing the known sources, sinks, and
  *              other interesting methods and sets up the sets of method defs
  *              for each of these.
  *
  * @param fn      a string naming the XML file to read for known sources,
  *                sinks, and other interesting methods.
  * @param clazzes an Array of ClassDef objects that represents all the know
  *                classes in the program to be analyzed.
  */
class SourceSinkMethodCallAnalyzer(fn: String, simpleCallGraph: SimpleMethodCallGraph) {
  /* The following list comes from a standard list of sources and sinks.  Some
   * of these seem less like a source or sink procedure, then it does a
   * combination of source and sink methods with constructors for objects that
   * are associated with some (but not all) of them.  Also note, that this list
   * does not differentiate the method by type, only by name.  This is based on
   * the simplifying assumption that if one version of the method leaks, all of
   * the methods of this name leak. */

  private val xmlFile = XML.loadFile(fn)
  private def buildSet(root: NodeSeq, classMap: Map[String,ClassDefProxy]):
    Set[MethodDef] = {
    root.foldLeft(Set.empty[MethodDef]) {
      (s, node) => (node \ "method").foldLeft(s) {
        (s, node) => {
          val className = "L" +
            (node \ "@class-name").toString.replace(".", "/") + ";"
          val methodName = (node \ "@name").toString
          if (classMap isDefinedAt className) {
            val cdp = classMap(className)
            if (cdp.methodMap isDefinedAt methodName)
              cdp.methodMap(methodName).calledBy.foldLeft(s) {
                (s, mdp) => if (mdp.methodDef == null) s else s + mdp.methodDef
              }
            else 
              s
          } else {
            s
          }
        }
      }
    }
  }

  private var _sources = buildSet(xmlFile \ "sources", simpleCallGraph.classMap)
  private var _sinks   = buildSet(xmlFile \ "sinks", simpleCallGraph.classMap)
  private var _other   = buildSet(xmlFile \ "other", simpleCallGraph.classMap)

  /** Accessor to return the Set of MethodDefs that call source methods.
    *
    * @returns the set of methods that call known source methods.
    */
  def sources = _sources

  /** Accessor to return the Set of MethodDefs that call sink methods.
    *
    * @returns the set of methods that call known sink methods.
    */
  def sinks   = _sinks

  /** Accessor to return the Set of MethodDefs that call other intersting
    * methods.
    *
    * @returns the set of methods that call known other interesting methods.
    */
  def other   = _other
}
