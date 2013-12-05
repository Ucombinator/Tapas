package org.ucombinator.webapp.auth.strategies

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

import org.scalatra.ScalatraBase
import org.scalatra.auth.ScentryStrategy

import org.ucombinator.webapp.model.User
import org.ucombinator.webapp.db.Users

import scala.slick.session.Database
import Database.threadLocalSession

class UserPasswordStrategy(protected val app: ScalatraBase)(implicit request: HttpServletRequest, respons: HttpServletResponse)
  extends ScentryStrategy[User] {

  override def name = "UserPassword"

  private def username = app.params.getOrElse("username", "")
  private def password = app.params.getOrElse("password", "")

  override def isValid(implicit request: HttpServletRequest) = username != "" && password != ""

  def authenticate()(implicit request: HttpServletRequest, response: HttpServletResponse): Option[User] = 
    Users.checkPassword(username, password)
}
