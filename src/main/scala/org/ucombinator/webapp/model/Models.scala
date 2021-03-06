package org.ucombinator.webapp.model

import java.sql.Timestamp

import org.ucombinator.webapp.db._

case class User(id: Int, name: String, username: String, email: Option[String], isAdmin: Boolean)

object User {
  def apply(id: String) = Users.lookupUserById(id.toInt) match {
    case Some(u) => u
    case None => throw new Exception("Did not find user for given id")
  }
}

case class AndroidApp(id: Int, user: User, appName: String, fileLocation: String, uploadDate: Timestamp)
