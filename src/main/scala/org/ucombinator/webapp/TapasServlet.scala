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

class TapasServlet(val db: Database) extends TapasWebAppStack with AuthenticationSupport with FileUploadSupport with FlashMapSupport {
  configureMultipartHandling(MultipartConfig(maxFileSize = Some(3 * 1024 * 1024)))

  before() {
    val reqpath = requestPath
    // not sure if this is the best way to do this, it is mostly useful because
    // it is easier to serve the static content through Scalatra.
    if (! (reqpath.startsWith("/css") ||
           reqpath.startsWith("/js") ||
           reqpath.startsWith("/img"))) {
      db withSession {
        requireLogin()
      }
    }
  }

  get("/") {
    contentType = "text/html"

    db withSession {
      ssp("index",
        ("apps", AndroidApps.getApps(user)),
        ("title", "Welcome to Semantic Grep"),
        ("isSignedIn", true), ("isAdmin", user.isAdmin))
    }
  }

  get("/upload") {
    contentType = "text/html"
    db withSession {
      ssp("upload",
        ("flash", flash),
        ("title", "Upload an APK file"),
        ("isSignedIn", true), ("isAdmin", user.isAdmin))
    }
  }

  get("/analyze/:id") {
    db withSession {
      val maybeApp = AndroidApps.getApp(params("id").toInt)
      maybeApp match {
        case None => redirect("/appError")
        case Some(app) => {
          val simpleCallGraph = new SimpleMethodCallGraph(new ApkReader(app.fileLocation).readFile)
          val config = new SourceSinkConfig("config/sourceSink.xml")
          contentType = "text/html"
          ssp("analyze",
              ("sourcesAndSinks", new SourceSinkMethodCallAnalyzer(
                                    config, simpleCallGraph,
                                    Set.empty[Symbol],   // limitToCategories
                                    Map.empty[Symbol,Int])),
              ("title", "Upload an APK file"),
              ("isSignedIn", true), ("isAdmin", user.isAdmin))
        }
      }
    }
  }

  post("/upload") {
    println("about to look at thefile")
    println("keys: " + params.keys.mkString(", "))
    params.foreach { (a) => println(a._1 + ": " + a._2) }
    fileParams.get("thefile") match {
      case Some(file) => {
        println("we got something forwarding to analyze")
        val dir = new File("tmp" + File.separator + user.username)
        if (!dir.exists) dir.mkdirs
        val apkFile = new File(dir + File.separator + file.name)
        file.write(apkFile)

        db withSession {
          val id = AndroidApps.addApp(user, params.getOrElse("appname", ""), apkFile.toString)
          redirect("/analyze/" + id)
        }
      }
  
      case None => {
        println("looks like we didn't get anything")
        flash("upload.error") = "Please supply an APK file to analyze"
        contentType = "text/html"
        db withSession {
          ssp("upload",
              ("flash", flash),
              ("title", "Upload an APK file"),
              ("isSignedIn", true), ("isAdmin", user.isAdmin))
        }
      }
    }
  }
}
