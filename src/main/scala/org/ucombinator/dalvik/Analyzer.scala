package org.ucombinator.dalvik

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import java.io.{PrintStream, FileOutputStream, File, FileReader}
import io.Source
import collection.SortedSet
import org.ucombinator.dalvik.android.ApkReader
import org.ucombinator.dalvik.AST._
import org.ucombinator.dalvik.analysis.{SimpleMethodCallGraph, SourceSinkMethodCallAnalyzer, SourceSinkConfig}
import annotation.tailrec

object Analyzer extends App {
  var apkFile: String = null
  var dump = false
  var outputFile: String = null
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
            reportError("APK file is already set to " + apkFile +
                        " and we can only process on file at a time now, so " +
                        fn + " cannot also be processed")
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
  config.addMethods(additionalMethods)

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
                mdp => {
                  val m = if (mdp.method == null)
                            mdp.methodDef.method
                          else
                            mdp.method
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

  // Look, a real, if (very, very) simple, analyzsis
  val sourcesAndSinks = new SourceSinkMethodCallAnalyzer(config,
                          simpleCallGraph, limitToCategories, costSpecification)
  def printMethodsAndSources(mds: Set[MethodDef]) {
    mds foreach {
      (md) => println("  " + md.method.classType.toS + "." + md.name +
                (md.sourceLocation match {
                   case Some((fn,line,pos)) => " (" + fn + ":" + line + ") pos: " + pos
                   case None => ""
                   }))
    }
  }
  def printMethodsWithCostAndSources(mds: SortedSet[(Int,MethodDef)]) {
    mds foreach {
      (a) => println("  " + a._1 + "\t" + a._2.method.classType.toS + "." + a._2.name +
               (a._2.sourceLocation match {
                  case Some((fn,line,pos)) => " (" + fn + ":" + line + ") pos: " + pos
                  case None => ""
                  }))
    }
  }
  wrapOutput {
    /* setting this aside in favor of the cost-sourted analysis */
    // println("Methods that call sources (non-exhaustive): ")
    // printMethodsAndSources(sourcesAndSinks.sources)
    // println
    // println("Methods that call sinks (non-exhaustive): ")
    // printMethodsAndSources(sourcesAndSinks.sinks)
    // println
    // println("Methods that call other interesting methods (non-exhaustive): ")
    // printMethodsAndSources(sourcesAndSinks.other)
    // println
    println("Methods that call sources or sinks (higher numbers indicate more hits): ")
    printMethodsWithCostAndSources(sourcesAndSinks.methodCosts)
    println
  }
}
