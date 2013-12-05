package org.ucombinator.webapp.auth.strategies

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

import org.scalatra.ScalatraBase
import org.scalatra.auth.ScentryStrategy

import org.ucombinator.webapp.model.User

import scala.slick.session.Database
import Database.threadLocalSession

class SignUpStrategy(protected val app: ScalatraBase)(implicit request: HttpServletRequest, respons: HttpServletResponse)
  extends ScentryStrategy[User] {

  override def name = "SignUp"

  private def signUpUser: Option[User] = app.session.getAttribute("signup.user") match {
                                           case u: User => Some(u)
                                           case _ => None
                                         }
                                  

  override def isValid(implicit request: HttpServletRequest) =
    app.session.getAttribute("signup.user") != null

  def authenticate()(implicit request: HttpServletRequest, response: HttpServletResponse): Option[User] =
    signUpUser
}
