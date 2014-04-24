package org.ucombinator.dalvik

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import java.io.{PrintStream, FileOutputStream, File, FileReader}
import io.Source
import collection.SortedSet
import org.ucombinator.dalvik.android.ApkReader
import org.ucombinator.dalvik.AST._
import org.ucombinator.dalvik.analysis.{SimpleMethodCallGraph, SourceSinkMethodCallAnalyzer}
import org.ucombinator.dalvik.analysis.{MethodDefProxy, SourceSinkConfig, MethodConfig, ClassConfig}
import annotation.tailrec
import spray.json._
import DefaultJsonProtocol._
import java.io.{FileWriter, PrintWriter}

/**
 * Used for reading/writing to database, files, etc.
 * Code From the book "Beginning Scala"
 * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
 */

object FileUtils {
	def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
	try { f(param) } finally { param.close() }
	
	def writeToFile(fileName:String, data:String) = 
	  using (new FileWriter(fileName)) {
	    fileWriter => fileWriter.write(data)
	  }
}

//class JsItem
//class JsObject(val properties : Map[String,JsItem]) extends JsItem
//class JsArray(val items : List[JsItem]) extends JsItem
//class JsNumber(val value : Long) extends JsItem
//class JsString(val str : String) extends JsItem

object Analyzer extends App {
  var apkFile: String = null
  var dump = false
  var outputFile: String = null
  var outputDirectory: String = ""
  var databaseFile: String = null
  var configFile: String = "config/sourceSink.xml" // set our default file location
  var className: String = null
  var methodName: String = null
  var listCategories = false
  var additionalMethods = List.empty[(String, String, String)]
  var costSpecification = Map.empty[Symbol,Int]
  var limitToCategories = Set.empty[Symbol]

  private def displayHelpMessage = {
    println("usage: analyzer [<options>] APK-file")
    println("  -h | --help              :: print this message")
    println("  -d | --dump              :: dump out the class definitions")
    println("  -o | --output-file       :: set the file for dump")
    println("  -c | --class-name        :: indicate the class name to analyze")
    println("  -m | --method-name       :: indicate the method name to analyze")
    println("  -f | --config            :: set the configuration file")
    println("  -l | --list-categories   :: list the known categories of sources and sinks")
    println("  -g | --limit-categories  :: limit report to only contain certain categories")
    println("  -G | --categories-file   :: limit report to categories in file")
    println("  -a | --add-methods       :: allows adding a set of methods w/categories")
    println("  -A | --add-method-file   :: specify a filename of additional methods w/categories")
    println("  -s | --specify-cost      :: specifies cost by category")
    println("  -S | --specify-cost-file :: specify a filename of costs by category")
    sys.exit
  }

  private def reportError(msg: String) = {
    println(msg)
    println("run with --help for command syntax")
    sys.exit
  }

  @tailrec
  private def parseOptions(args:List[String]):Unit = {
    args match {
      case ("-h"  | "--help") :: rest => displayHelpMessage
      case ("-d"  | "--dump") :: rest => dump = true ; parseOptions(rest)
      case ("-o"  | "--output-file") :: fn :: rest => outputFile = fn ; parseOptions(rest)
      case ("-db" | "--database") :: fn :: rest => databaseFile = fn ; parseOptions(rest)
      case ("-c"  | "--class-name") :: cn :: rest => className = cn ; parseOptions(rest)
      case ("-f"  | "--config") :: fn :: rest => configFile = fn ; parseOptions(rest)
      case ("-m"  | "--method-name") :: mn :: rest => methodName = mn ; parseOptions(rest)
      case ("-l"  | "--list-categories") :: rest => listCategories = true ; parseOptions(rest)
      case ("-g"  | "--limit-categories") :: rest => val rest2 = limitCategories(rest) ; parseOptions(rest2)
      case ("-G"  | "--categories-file") :: fn :: rest => readCategoriesFile(fn) ; parseOptions(rest)
      case ("-a"  | "--add-methods") :: rest => val rest2 = additionalMethods(rest) ; parseOptions(rest2)
      case ("-A"  | "--add-method-file") :: fn :: rest => readAdditionalMethods(fn) ; parseOptions(rest)
      case ("-s"  | "--specify-cost") :: rest => val rest2 = parseCostSpecification(rest) ; parseOptions(rest2)
      case ("-S"  | "--specify-cost-file") :: fn :: rest => readCostSpecification(fn) ; parseOptions(rest)
      case fn :: rest => {
        if (apkFile == null) {
          //println(fn)
          val f = new File(fn)

          if (f.exists) {
            if (f.isFile) {
              if (f.canRead) {
                apkFile = fn 
                parseOptions(rest)
              } else {
                reportError(fn + " is not a readable file")
              }
            } else {
              reportError(fn + " is not a file")
            }
          } else {
            if (fn(0) == '-') {
              reportError("Unrecognized option " + fn)
            } else {
              reportError(fn + " does not exist")
            }
          }
        } else {
          if (fn(0) == '-') {
            reportError("Unrecognized option " + fn)
          } else {
              val outDirectory = new File(fn);
              // if the directory does not exist, create it
              if (!outDirectory.exists())
              {
                System.out.println("creating directory: " + fn);
                outDirectory.mkdir();
              }
              outputDirectory = if(fn.endsWith("/")) fn else fn + "/";
          } 
        }
      }
      case Nil => Unit
      case _ => println("unrecognized option: " + args) ; displayHelpMessage
    }
  }

  object CLIFancyOptionsParser extends JavaTokenParsers {
    override val whiteSpace = """[ \t\n,]+""".r

    private def category: Parser[String] = """\w+""".r
    private def cost: Parser[Int] = """\d+""".r ^^ { _.toInt }
    private def fqname: Parser[Array[String]] = ident ~ rep("[.$#:]".r ~ ident) ^^ {
      case id ~ ls => {
        ls.foldLeft(Array(id))((a, sepid) => sepid match {
          case sep ~ id => a :+ id
        })
      }
    }
    private def limitCategories: Parser[Set[Symbol]] =
      """\{""".r ~ rep(category) ~ "}".r ^^ {
      case o ~ ls ~ c => ls.map(str => Symbol(str)).toSet
    }
    private def addMethods: Parser[List[(String,String,String)]] =
      """\{""".r ~ rep(fqname ~ category) ~ "}".r ^^ {
      case o ~ ls ~ c => {
        ls.map(mcat => mcat match {
          case m ~ cat => {
            val (clazz, method) = generateCanonicalMethod(m)
            (clazz, method, cat)
          }
        })
      }
    }
    private def specifyCost: Parser[Map[Symbol, Int]] =
      """\{""".r ~ rep(category ~ cost) ~ "}".r ^^ {
      case o ~ ls ~ c => {
        ls.foldLeft(Map[Symbol,Int]())((m, catcost) => catcost match { 
          case cat ~ cost => m + (Symbol(cat) -> cost)
        })
      }
    }
    private def generateCanonicalMethod(fqname: Array[String]): (String, String) = {
      val len = fqname.length
      ("L" + fqname.take(len - 1).mkString("/") + ";", fqname(len - 1))
    }
    private def gatherParts(args:List[String]): (List[String], String) = {
      @tailrec
      def gather(args: List[String], option: String): (List[String], String) = {
        val parts = new Regex("(.*})(.+)", "opt", "remainder")
        args match {
          case s :: rest if s.matches(".*}") => (args.tail, option + " " + s)
          case parts(opt,remainder) :: rest =>
            (remainder :: args.tail, option + " " + opt)
          case s :: rest => gather(args.tail, option + " " + s)
          case Nil => (args, option)
        }
      }
      gather(args, "")
    }
    def parseLimitCategories(input: List[String]): (Set[Symbol], List[String]) = {
      val (rest, str) = gatherParts(input)
      parseAll(limitCategories, str) match {
        case Success(result, _) => (result, rest)
        case failure : NoSuccess => scala.sys.error(failure.msg)
      }
    }
    def parseSpecifyCost(input: List[String]): (Map[Symbol, Int], List[String]) = {
      val (rest, str) = gatherParts(input)
      parseAll(specifyCost, str) match {
        case Success(result, _) => (result, rest)
        case failure : NoSuccess => scala.sys.error(failure.msg)
      }
    }
    def parseAddMethods(input: List[String]): (List[(String, String, String)], List[String]) = {
      val (rest, str) = gatherParts(input)
      parseAll(addMethods, str) match {
        case Success(result, _) => (result, rest)
        case failure : NoSuccess => scala.sys.error(failure.msg)
      }
    }
    def parseLimitCategoriesFile(fn: String): Set[Symbol] =
      parseAll(limitCategories, new FileReader(fn)) match {
        case Success(result, _) => result
        case failure : NoSuccess => scala.sys.error(failure.msg)
      }
    def parseSpecifyCostFile(fn: String): Map[Symbol, Int] =
      parseAll(specifyCost, new FileReader(fn)) match {
        case Success(result, _) => result
        case failure : NoSuccess => scala.sys.error(failure.msg)
      }
    def parseAddMethodsFile(fn: String): List[(String, String, String)] =
      parseAll(addMethods, new FileReader(fn)) match {
        case Success(result, _) => result
        case failure : NoSuccess => scala.sys.error(failure.msg)
      }
  }

  private def readAdditionalMethods(fn:String): Unit =
    additionalMethods ++= CLIFancyOptionsParser.parseAddMethodsFile(fn)

  private def readCostSpecification(fn:String): Unit =
    costSpecification ++= CLIFancyOptionsParser.parseSpecifyCostFile(fn)

  private def readCategoriesFile(fn:String): Unit =
    limitToCategories ++= CLIFancyOptionsParser.parseLimitCategoriesFile(fn)

  private def additionalMethods(args:List[String]): List[String] = {
    val (m, rest) = CLIFancyOptionsParser.parseAddMethods(args)
    additionalMethods ++= m
    rest
  }

  private def parseCostSpecification(args:List[String]): List[String] = {
    val (cs, rest) = CLIFancyOptionsParser.parseSpecifyCost(args) 
    costSpecification ++= cs
    rest
  }

  private def limitCategories(args:List[String]): List[String] = {
    val (lc, rest) = CLIFancyOptionsParser.parseLimitCategories(args)
    limitToCategories ++= lc
    rest
  }

  private def printFields(fieldType: String, fields: Array[FieldDef]): Unit = {
    if (fields != null && fields.length > 0) {
      println("  " + fieldType + " fields:")
      for(f <- fields) {
        val fld = f.field
        println("    " + fld.fieldType.toS + " " + fld.name)
      }
    }
  }

  private def printMethods(methodType: String, methods: Array[MethodDef]): Unit = {
    if (methods != null && methods.length > 0) {
      println("  " + methodType + " methods:")
      for(m <- methods) {
        val meth = m.method
        val proto = meth.prototype
        println("    " + proto.returnType.toS + " " + meth.name + "(" +
          (if (proto.parameters == null)
             ""
           else
             (proto.parameters map { p => p.toS }).mkString(", ")) + ")")
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
        println("      " + idx + ":\t" + ci.insns(idx).toS())
      }
    }
  }

  private def dumpClassDefs(classDefs: Array[ClassDef]): Unit = {
    for (cd <- classDefs) {
      print("class: " + cd.toS)
      if (cd.superClass != null) {
        val superName = cd.superClass.toS
        if (superName != "java.lang.Object")
          print(" (super: " + superName + ")")
      }
      println
      if (cd.interfaces != null)
        println("  interfaces: " + (cd.interfaces map { i => i.toS }).mkString(", "))
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

  // first step at separting out the category information.
  val config = new SourceSinkConfig(configFile)
  config.addOtherMethods(additionalMethods)

  if (apkFile == null) {
    if (listCategories) {
      println("Categories: ")
      println("  " + config.categories.map { (sym) => sym.toString }.mkString(", "))
      sys.exit
    } else {
      displayHelpMessage
    }
  }

  val apkReader = new ApkReader(apkFile)
  val classDefs = apkReader.readFile

  if (dump) wrapOutput { dumpClassDefs(classDefs) }

  val simpleCallGraph = new SimpleMethodCallGraph(classDefs)

  if (className != null && methodName != null) {
    wrapOutput {
      println(
        (if (simpleCallGraph.classMap isDefinedAt className) {
           val cdp = simpleCallGraph.classMap(className)
           if (cdp.methodMap isDefinedAt methodName) {
             (cdp.methodMap(methodName).calls map {
                calledAt => {
                  val (callee, callSite) = calledAt
                  val m = if (callee.method == null)
                            callee.methodDef.method
                          else
                            callee.method
                  m.classType.toS + "." + m.name
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

  // Look, a real, if (very, very) simple, analysis
  val sourcesAndSinks = new SourceSinkMethodCallAnalyzer(config,
                          simpleCallGraph, limitToCategories, costSpecification)

   var costs = Map[String, Int]().empty;
   sourcesAndSinks.methodCosts.toList.foreach((costPair) => {
     val (cost, mdp) = costPair
     val method = if(mdp.method != null) mdp.method else mdp.methodDef.method
     if(method != null)
       costs += (method.fullyQualifiedName -> cost)
     else
       println("ERROR: method mis undefined in methodCosts")
       
   })
   
   // TODO restructure the config to be better suited to testing membership
   var sinks = Map[String, MethodConfig]().empty;
   config.sinkMap.toList.foreach((classCfgPair) => {
     val (className, classCfg) = classCfgPair
     
     classCfg.methods.toList.foreach((meth) => {
       sinks += (meth.fullyQualifiedName -> meth)
     })
   })
	   
   var sources = Map[String, MethodConfig]().empty;
   config.sourceMap.toList.foreach((classCfgPair) => {
     val (className, classCfg) = classCfgPair
     
     classCfg.methods.toList.foreach((meth) => {
       sources += (meth.fullyQualifiedName -> meth)
     })
   })
   
   var otherMethods = Map[String, MethodConfig]().empty;
   config.otherMap.toList.foreach((classCfgPair) => {
     val (className, classCfg) = classCfgPair
     
     classCfg.methods.toList.foreach((meth) => {
       otherMethods += (meth.fullyQualifiedName -> meth)
     })
   })
  
  
  def printMethodsAndSources(mds: Set[MethodDef]) {
    mds foreach {
      (md) => println("  " + md.method.classType.toS + "." + md.name +
                (md.sourceLocation match {
                   case Some((fn,line,pos)) => " (" + fn + ":" + line + ") pos: " + pos
                   case None => ""
                   }))
    }
  }
  
  def printMethodsWithCostAndSources(mds: SortedSet[(Int,MethodDefProxy)]) {
  
  }
  
  def renderCallGraph(mds: SortedSet[(Int,MethodDefProxy)]) : String = {
    val callGraph = JsObject(mds.foldLeft(Map[String, JsValue]().empty) { 
      (mappings, riskPair) => {
       val (risk, mdp) = riskPair
       val md = if(mdp.methodDef != null) mdp.methodDef else new MethodDef(mdp.method)
       val (filename : String, line : Long, pos : Long)  = md sourceLocation match {
         case Some((fn,line,pos)) => (fn, line, pos)
         case None => ("none", -1, -1)
       }
       mappings + (md.method.fullyQualifiedSignature.toString() -> JsObject(
           "file"  -> JsString(filename),
           "line"  -> JsNumber(line),
           "col"   -> JsNumber(pos),
           "calls" -> JsArray(mdp.calls.map((calledAt) => {
              val (calleeProxy, callSite) = calledAt
              val calleeMethod = if(calleeProxy.methodDef != null) calleeProxy.methodDef.method else calleeProxy.method
              JsObject("method" -> JsString(calleeMethod.fullyQualifiedSignature),
                       "line"   -> JsNumber(callSite.line),
                       "col"    -> JsNumber(callSite.position))
            }).toList)
        ))
      }
    })
    JsObject("call_graph" ->  callGraph).prettyPrint
  }
  
  def renderAnnotations(mds: SortedSet[(Int,MethodDefProxy)]) : String = {
    val annotations = JsArray((mds map {
      (a) => {
           val (risk, mdp) = a
           val md = if(mdp.methodDef != null) mdp.methodDef else new MethodDef(mdp.method)
           val (fn : String, line : Long, pos : Long)  = md sourceLocation match {
             case Some((fn,line,pos)) => (fn, line, pos)
             case None => ("none", -1, -1)
           }
           val fullName  = md.method.fullyQualifiedName
           val isSource  = sources.isDefinedAt(fullName)
           val longDesc  = if(isSource) sources(fullName).category.name.toString() else "other"
           val shortDesc = if(isSource) "source" else ""
           JsObject("risk_score"        -> JsNumber(risk), 
                    "method"            -> JsString(md.name),
                    "file_name"         -> JsString(fn),
                    "class_name"        -> JsString(md.method.classType.toS),
                    "short_description" -> JsString(shortDesc),
                    "long_description"  -> JsString(longDesc),
                    "start_line"        -> JsNumber(line),
                    "start_col"         -> JsNumber(pos),
                    "sub_annotations"   -> JsArray(mdp.calls.filter((calledAt) => {
                       val (callee, callSite) = calledAt
                       val fullCalleeName = if(callee.methodDef != null) { 
                         callee.methodDef.method.fullyQualifiedName
                       } else { 
                         callee.method.fullyQualifiedName
                       }
                       (sinks.isDefinedAt(fullCalleeName) ||
                        sources.isDefinedAt(fullCalleeName) ||
                        otherMethods.isDefinedAt(fullCalleeName))
                    }).map(((calledAt) => {
                       val (callee, callSite) = calledAt
                       val method = if(callee.methodDef != null) callee.methodDef.method else callee.method 
                       val fullNm = method.fullyQualifiedName
                       val calleeCost = if(costs.isDefinedAt(fullNm)) costs(fullNm) else MethodConfig.DEFAULT_COST
                       val category = if(sinks.isDefinedAt(fullNm)) 
				                         sinks(fullNm).category.name.toString();
				                      else if(sources.isDefinedAt(fullNm)) 
				                         sources(fullNm).category.name.toString()
				                      else if(otherMethods.isDefinedAt(fullNm)) 
				                        otherMethods(fullNm).category.name.toString()
				                      else ""
			                                        
                       JsObject("start_line"  -> JsNumber(callSite.line),
								"end_line"    -> JsNumber(callSite.line),
								"start_col"   -> JsNumber(callSite.position),
								"method"      -> JsString(method.name),
								"class_name"  -> JsString(method.classType.toS),
								"risk_score"  -> JsNumber(calleeCost), 
								"description" -> JsString(category))
                    })).toList))
      }
    }).toList)
    
    JsObject("annotations" -> annotations).prettyPrint
  }
  wrapOutput {
    /* setting this aside in favor of the cost-sorted analysis */
    // println("Methods that call sources (non-exhaustive): ")
    // printMethodsAndSources(sourcesAndSinks.sources)
    // println
    // println("Methods that call sinks (non-exhaustive): ")
    // printMethodsAndSources(sourcesAndSinks.sinks)
    // println
    // println("Methods that call other interesting methods (non-exhaustive): ")
    // printMethodsAndSources(sourcesAndSinks.other)
    // println
  }
  
  val apkName = if (apkFile != null) {
      apkFile.split('/').last.replace(".apk", "").toLowerCase()
  }
  
  FileUtils.writeToFile(outputDirectory + apkName + "_tapas_callgraph.json", 
      renderCallGraph(sourcesAndSinks.methodCosts))
  FileUtils.writeToFile(outputDirectory + apkName + "_tapas_risk.json", 
      renderAnnotations(sourcesAndSinks.methodCosts))
}
