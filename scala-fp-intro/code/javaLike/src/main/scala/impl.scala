package intro

class RESTService(user: String, password: String) extends Service {

  private[this] val transaction = new Transaction()

  def commit() = transaction.commit()
  def rollback() = transaction.rollback()

  def getBalance(account: Long) = {
    makeRequest("GET", s"accounts/${account}")
    12.0
  }

  def creditAccount(account: Long, amount: Double) = {
    makeRequest("POST", s"accounts/${account}/credits")
    12.0 + amount
  }

  def accounts() = {
    makeRequest("GET", "accounts")
    1L :: 2L :: Nil
  }

  private[this] def makeRequest(method: String, suffix: String) = {
    println(s"${method} request to /transactions/${transaction.id}/${suffix}")
  }

  private[this] class Transaction {
    println("POST request to /transactions")
    val id = 1L

    def commit() = {
      println(s"PUT reuqest to /transactions/${id}/commit")
    }

    def rollback() = {
      println(s"DELETE reuqest to /transactions/${id}/commit")
    }
  }
}
