package org.ucombinator.webapp.auth

import org.scalatra.auth.{ScentryConfig, ScentrySupport}
import org.scalatra.{ScalatraBase}
import org.ucombinator.webapp.auth.strategies._
import org.ucombinator.webapp.model.User

trait AuthenticationSupport extends ScalatraBase with ScentrySupport[User] {
  self: ScalatraBase =>

  protected def fromSession =  {
    case id =>  User(id)
  }

  protected def toSession = {
    case User(id, name, username, email, admin) => id.toString
  }

  protected val scentryConfig = (new ScentryConfig {
    override val login = "/account/login"
  }).asInstanceOf[ScentryConfiguration]

  protected def requireLogin() = if (!isAuthenticated) redirect(scentryConfig.login)

  override protected def configureScentry = 
    scentry.unauthenticated {
      scentry.strategies("UserPassword").unauthenticated()
    }

  override protected def registerAuthStrategies = {
    scentry.register("UserPassword", app => new UserPasswordStrategy(app))
    scentry.register("SignUp", app => new SignUpStrategy(app))
  }
}
