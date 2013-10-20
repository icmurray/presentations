package uk.co.sprily.scalaTalk

import akka.actor.{ActorSystem, Props, Actor}
import akka.io.IO
import spray.can.Http

object Boot extends App {

  // we need an ActorSystem to host our application in
  implicit val system = ActorSystem("on-spray-can")

  // create and start our service actor
  val service = system.actorOf(Props[ServiceActor], "hello-service")

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ! Http.Bind(service, "localhost", port = 8080)
}

class ServiceActor extends Actor
                      with PrivateRoutes
                      with UserRoutes
                      with SlickUserServiceModule
                      with SlickUserTableModule
                      with InMemoryDatabaseModule {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing,
  // timeout handling or alternative handler registration
  def receive = runRoute(allRoutes)

  lazy val allRoutes = userRoutes ~ privateRoutes
}
