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
  * @param cfg     a SourceSinkConfig object with the XML read stuff.
  * @param clazzes an Array of ClassDef objects that represents all the know
  *                classes in the program to be analyzed.
  */
class SourceSinkMethodCallAnalyzer(ssc: SourceSinkConfig, simpleCallGraph: SimpleMethodCallGraph) {

  // TODO: to finish this we need to:
  // a) have a version of this that will take a set of categories
  // b) make it use the set of categories pair down the list of methods to report on
  // c) adjust the buildSet function to use the map.
  // ------
  // d) have it take costs for each of the categories
  // e) have it return a sorted list of costs and method defs

  private def buildSet(m: Map[String,SourceSinkConfig#ClassConfig],
                       classMap: Map[String,ClassDefProxy]):
    Set[MethodDef] = {
    m.foldLeft(Set.empty[MethodDef]) {
      (s, a) => a._2.methods.foldLeft(s) {
        (s, mc) => {
          val className = a._1
          val methodName = mc.name
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

  private var _sources = buildSet(ssc.sourceMap, simpleCallGraph.classMap)
  private var _sinks   = buildSet(ssc.sinkMap, simpleCallGraph.classMap)
  private var _other   = buildSet(ssc.otherMap, simpleCallGraph.classMap)

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
