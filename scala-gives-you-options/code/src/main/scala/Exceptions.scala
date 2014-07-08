object ExceptionExample {

  def trySomething(): Int = {
    try {
      throw new java.io.IOException("Uh Oh")
    } catch {
      case (e: java.io.IOException) => 0
      case (e: ArithmeticException) => 1
    }
  }

  def trySomethingElse(): Int = {
    try {
      throw new ArithmeticException("Uh Oh")
    } catch handleIOException orElse handleMathException
  }

  private def handleIOException: PartialFunction[Throwable, Int] = {
    case (e: java.io.IOException) => 0
  }

  private def handleMathException: PartialFunction[Throwable, Int] = {
    case (e: ArithmeticException) => 1
  }

}
