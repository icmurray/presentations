package intro

object Main {
  def main(args: Array[String]): Unit = {
    val session = new RESTService("ian", "password")
    session.getBalance(1L)
    session.debitAccount(1L, 100.0)
    session.fineOverdrawn(10.0)
    session.commit()
  }
}
