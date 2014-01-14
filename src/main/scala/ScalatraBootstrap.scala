import org.ucombinator.webapp._
import org.ucombinator.webapp.db._
import org.scalatra._
import javax.servlet.ServletContext
import com.mchange.v2.c3p0.ComboPooledDataSource
import scala.slick.jdbc.meta.MTable
import scala.slick.session.Database
import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession


class ScalatraBootstrap extends LifeCycle {
  val cpds = new ComboPooledDataSource

  override def init(context: ServletContext) = {
    val db = Database.forDataSource(cpds)
    db withSession {
      // a more sophisticated process is probably needed to
      // get this working in the long run.
      if (MTable.getTables("USERS").list.isEmpty) {
        Users.ddl.create
        Users.addUser("Admin User", "root", None, "ucomb2013", true)
      }
      if (MTable.getTables("ANDROID_APPS").list.isEmpty) AndroidApps.ddl.create
    }
    context.mount(new TapasServlet(db), "/*")
    context.mount(new LoginServlet(db), "/account/*")
  }

  private def closeDbConnection() = cpds.close

  override def destroy(context: ServletContext) = {
    super.destroy(context)
    closeDbConnection
  }
}
