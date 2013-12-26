package org.ucombinator.webapp

import org.scalatra._
import org.scalatra.servlet.{FileUploadSupport, MultipartConfig}
import scalate.ScalateSupport
import scala.slick.session.Database
import java.io.File

import org.ucombinator.webapp.auth.AuthenticationSupport

import org.ucombinator.webapp.db.AndroidApps

import org.ucombinator.dalvik.android.ApkReader
import org.ucombinator.dalvik.analysis.{SimpleMethodCallGraph, SourceSinkMethodCallAnalyzer, SourceSinkConfig}

class TapasServlet(val db: Database) extends TapasWebAppStack with AuthenticationSupport with FileUploadSupport {
  configureMultipartHandling(MultipartConfig(maxFileSize = Some(3 * 1024 * 1024)))

  before() {
    db withSession {
      requireLogin()
    }
  }

  get("/") {
    contentType = "text/html"

    db withSession {
      ssp("/index",
        ("apps", AndroidApps.getApps(user)),
        ("title", "Welcome to Semantic Grep"),
        ("isSignedIn", true), ("isAdmin", user.isAdmin))
    }
  }

  get("/upload") {
    <html>
      <head>
        <title>Tapas File Upload Form</title>
      </head>
      <body>
        <h1>Please Upload an APK file</h1>
        <p>{System.getProperty("user.name")}</p>
        <p>{System.getProperty("user.dir")}</p>
        <form method="post" enctype="multipart/form-data">
          <input type="text" name="appname" placeholder="App Name" />
          <input type="file" name="thefile" />
          <input type="submit" value="Upload App" />
        </form>
      </body>
    </html>
  }

  post("/upload") {
    fileParams.get("thefile") match {
      case Some(file) => {
        val dir = new File("tmp" + File.separator + user.username)
        if (!dir.exists) dir.mkdirs
        val apkFile = new File(dir + File.separator + file.name)
        file.write(apkFile)
        val apkReader = new ApkReader(apkFile)
        val classDefs = apkReader.readFile
        val simpleCallGraph = new SimpleMethodCallGraph(classDefs)
        val configFile = "config/sourceSink.xml"
        val config = new SourceSinkConfig(configFile)
        val limitToCategories = Set.empty[Symbol]
        val costSpecification = Map.empty[Symbol,Int]
        val sourcesAndSinks = new SourceSinkMethodCallAnalyzer(
          config, simpleCallGraph, limitToCategories, costSpecification)

        db withSession {
          AndroidApps.addApp(user, params.getOrElse("appname", ""), apkFile.toString)
        }

        <html>
          <head>
            <title>Analyzer results</title>
          </head>
          <body>
            <h1>Analyzer results</h1>
            <p>Methods that call sources or sinks (higher numbers indicate more hits):</p>
            <table>
              <thead>
                <tr>
                  <th>Cost</th>
                  <th>Method name</th>
                  <th>File location (if available)</th>
                </tr>
              </thead>
              <tbody>
                {sourcesAndSinks.methodCosts map {
                   (a) => <tr>
                            <td>{a._1}</td>
                            <td>{a._2.method.classType.toS + "." + a._2.name}</td>
                            <td>{a._2.sourceLocation match {
                                   case Some((fn,line,pos)) =>
                                     fn + " at line: " + line + " pos: " + pos
                                   case None => ""
                                 }}</td>
                          </tr>
                   }
                }
              </tbody>
            </table>
          </body>
        </html>
      }
  
      case None =>
        BadRequest(
         <p>
            Hey! You forgot to select a file.
         </p>)
    }
  }
}
