package org.ucombinator.webapp

import org.scalatra._
import scalate.ScalateSupport
import scala.slick.session.Database

import org.ucombinator.webapp.auth.AuthenticationSupport
import org.ucombinator.webapp.db.Users

class LoginServlet(val db: Database) extends TapasWebAppStack with FlashMapSupport with AuthenticationSupport {
  get("/") {
    db withSession {
      if (isAuthenticated) {
        redirect("/")
      } else {
        redirect("/login")
      }
    }
  }

  get("/login") {
    <html>
      <body>
        <div id="flash">
          { flash.getOrElse("loginFail", "") }
          { flash.getOrElse("signupFail", "") }
        </div>
        <div id="signup">
          <form action="signup" method="post">
            <input type="text" name="name" placeholder="John Smith" />
            <input type="text" name="username" placeholder="jsmith" />
            <input type="email" name="email" placeholder="jsmith@bigbluebox.org" />
            <input type="password" name="password" placeholder="tardiskey" />
            <input type="password" name="password_confirm" placeholder="tardiskey" />
            <input type="submit" value="Sign Up!" />
          </form>
        </div>
        <div id="login">
           <form action="login" method="post">
             <input type="text" name="username" />
             <input type="password" name="password" />
             <input type="submit" value="Login" />
           </form>
        </div>
      </body>
    </html>
  }

  post("/login") {
    db withSession {
      scentry.authenticate()
      if (isAuthenticated) {
        redirect("/")
      } else {
        flash("loginFail") = "incorrect username or password"
        redirect("/login")
      }
    }
  }

  post("/signup") {
    // clean up the following with a simpler validation system
    var valid = true
    val name = params.get("name") match {
                 case Some(name) => name
                 case _ => { flash("signup.name") = "please specify a name"; valid = false; "" }
               }
    val username = params.get("username") match {
                     case Some(username) => {
                       db withSession {
                         if (! Users.isUsernameAvailable(username)) {
                           flash("signup.username") = "username is already in use, please seleect another"
                           valid = false
                         }
                       }
                       username
                     }
                     case _ => {
                       flash("signup.username") = "please specify a username"
                       valid = false
                       ""
                     }
                   }
    val email = params.get("email")
    val password = params.get("password") match {
                     case Some(password) => password
                     case _ => {
                       flash("signup.password") = "please specify a password"
                       valid = false
                       ""
                     }
                   }
    val password_confirm = params.get("password_confirm") match {
                             case Some(password_confirm) => {
                               if (password_confirm != password) {
                                 flash("signup.password_confirm") = "password and conrimation do not match"
                                 valid = false
                               }
                               password_confirm
                             }
                             case _ => {
                               flash("signup.password_confirm") = "please specify a password confirmation"
                               valid = false
                             }
                           }
    if (valid) {
      db withSession {
        val id = Users.addUser(name, username, email, password)
        val user = Users.lookupUserById(id)
        session.setAttribute("signup.user", user)
        scentry.authenticate()
      }
      redirect("/")
    } else {
      flash("signup.name.value") = name
      flash("signup.username.value") = username
      flash("signup.email.value") = email match { case Some(s) => s; case _ => "" }
      flash("signupFail") = "Unable to signup, please correct the following errors"
      redirect("/login")
    }
  }

  get("/logout") {
    db withSession { scentry.logout() }
    redirect("/")
  }
}
