package uk.co.sprily.scalaTalk

import scala.slick.lifted.DDL
import scala.slick.driver.ExtendedProfile

import com.typesafe.config._

trait DatabaseModule {
  val slickProfile: ExtendedProfile
  import slickProfile.simple._
  val database: Database

  def ddls: List[DDL] = Nil
}

trait InMemoryDatabaseModule extends DatabaseModule {
  import scala.slick.driver.H2Driver
  lazy val slickProfile = H2Driver

  import slickProfile.simple._

  lazy val database = {
    val db = Database.forURL(s"jdbc:h2:mem:bidding-${randomDBName};DB_CLOSE_DELAY=-1",
                             driver = "org.h2.Driver")
    if (ddls.nonEmpty) {
      val allDDLs = ddls.reduce(_ ++ _)
      db.withSession { implicit s: Session =>
        allDDLs.create
      }
    }

    db
  }

  import scala.util.Random
  import scala.List.fill

  private lazy val SEED = 10
  private lazy val r = new Random(SEED)

  private def randChar() = (r.nextInt('z'-'a' + 1) + 'a').toChar
  protected def randomDBName = fill(10) { randChar() } mkString("")
}

trait PostgresDatabaseModule extends DatabaseModule {
  import scala.slick.driver.PostgresDriver.simple._
  lazy val slickProfile = scala.slick.driver.PostgresDriver
  lazy val config = ConfigFactory.load()
  lazy val database = Database.forURL(
    config.getString("database.url"),
    driver = config.getString("database.driver"),
    user = config.getString("database.user"),
    password = config.getString("database.password"))
}
