import org.ucombinator.webapp._
import org.ucombinator.webapp.db._
import org.scalatra._
import javax.servlet.ServletContext
import com.mchange.v2.c3p0.ComboPooledDataSource
import scala.slick.session.Database
import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession


class ScalatraBootstrap extends LifeCycle {
  val cpds = new ComboPooledDataSource

  override def init(context: ServletContext) = {
    val db = Database.forDataSource(cpds)
    db withSession {
      // for right now create our DDL on startup because we are using an in
      // memory database.
      (Users.ddl ++ AndroidApps.ddl).create
      Users.addUser("Admin User", "root", "ucomb2013", true)
    }
    context.mount(new TapasServlet(db), "/*")
  }

  private def closeDbConnection() = cpds.close

  override def destroy(context: ServletContext) = {
    super.destroy(context)
    closeDbConnection
  }
}
