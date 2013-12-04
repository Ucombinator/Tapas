package org.ucombinator.webapp.db

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import org.ucombinator.webapp.util.Password

object Users extends Table[(Int, String, String, String, Boolean)]("USERS") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")         // the user's real name
  def email = column[String]("email")       // email address/username
  def password = column[String]("password") // password, which will be hashed
  def admin = column[Boolean]("admin")

  def * = id ~ name ~ email ~ password ~ admin

  // additional methods
  private def autoInc = id.? ~ name ~ email ~ password ~ admin

  def addUser(name: String, email: String, password: String, admin: Boolean = false) {
    autoInc.insert(None, name, email, Password(password), admin)
  }
}

object AndroidApps extends Table[(Int, Int, String, String)]("ANDROID_APPS") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def userId = column[Int]("user_id")
  def appName = column[String]("app_name")
  def fileLocation = column[String]("file_location")

  def * = id ~ userId ~ appName ~ fileLocation

  def user = foreignKey("user_fk", userId, Users)(_.id)
}
