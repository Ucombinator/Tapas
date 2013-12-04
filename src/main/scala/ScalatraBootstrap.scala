import org.ucombinator.webapp._
import org.scalatra._
import javax.servlet.ServletContext
import com.mchange.v2.c3p0.ComboPooledDataSource
import slick.session.Database

class ScalatraBootstrap extends LifeCycle {
  val cpds = new ComboPooledDataSource

  override def init(context: ServletContext) = {
    val db = Database.forDataSource(cpds)
    context.mount(new TapasServlet(db), "/*")
  }

  private def closeDbConnection() = cpds.close

  override def destroy(context: ServletContext) = {
    super.destroy(context)
    closeDbConnection
  }
}
