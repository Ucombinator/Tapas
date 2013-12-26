package org.ucombinator.webapp.db

import java.util.Date
import java.sql.Timestamp

import org.ucombinator.webapp.util.Password
import org.ucombinator.webapp.model._

import scala.slick.session.Session
import scala.slick.driver.H2Driver.simple._

import Database.threadLocalSession

object Users extends Table[(Int, String, String, Option[String], String, Boolean)]("USERS") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")         // the user's real name
  def username = column[String]("username") // username, because I actually prefer this to email addr
  def email = column[Option[String]]("email")       // email address/username
  def password = column[String]("password") // password, which will be hashed
  def admin = column[Boolean]("admin")

  def * = id ~ name ~ username ~ email ~ password ~ admin

  def usernameIdx = index("username_idx", username, unique = true)

  // additional methods
  private def autoInc = name ~ username ~ email ~ password ~ admin returning id

  def addUser(name: String, username: String, email: Option[String], password: String, admin: Boolean = false): Int =
    autoInc.insert(name, username, email, Password(password), admin)

  def lookupUserById(id: Int): Option[User] = 
    Query(Users).filter(_.id === id).firstOption match {
      case Some((id, name, un, email, pw, admin)) => Some(User(id, name, un, email, admin))
      case None => None
    }

  def isUsernameAvailable(username: String): Boolean =
    Query(Users).filter(_.username === username).firstOption match {
      case Some(u) => false
      case None => true
    }

  def checkPassword(username: String, password: String): Option[User] =
    Query(Users).filter(_.username === username).firstOption match {
      case Some((id, name, un, email, pw, admin)) =>
        if (pw == Password(password))
          Some(User(id, name, un, email, admin))
        else
          None
      case None => None
    }
}

object AndroidApps extends Table[(Int, Int, String, String, Timestamp)]("ANDROID_APPS") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def userId = column[Int]("user_id")
  def appName = column[String]("app_name")
  def fileLocation = column[String]("file_location")
  def fileUploadDate = column[Timestamp]("file_upload_date")

  def * = id ~ userId ~ appName ~ fileLocation ~ fileUploadDate

  def user = foreignKey("user_fk", userId, Users)(_.id)

  private def autoInc = userId ~ appName ~ fileLocation ~ fileUploadDate returning id

  def addApp(user: User, appName: String, fileLocation: String): Int =
    autoInc.insert(user.id, appName, fileLocation, new Timestamp(new Date().getTime()))

  def getApps(user: User): List[AndroidApp] =
    Query(AndroidApps).filter(_.userId === user.id).list map {
      (a) => AndroidApp(a._1, user, a._3, a._4, a._5)
    }
}
