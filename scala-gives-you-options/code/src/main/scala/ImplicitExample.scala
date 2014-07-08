trait Config { def port = 8080 }   // some sort of context

object ImplicitExample {
  def doSomething(i: Int)(implicit c: Config) {
    println(s"Doing something on port ${c.port}")
  }
}
