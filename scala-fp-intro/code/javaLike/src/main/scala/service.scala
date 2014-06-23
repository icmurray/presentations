package intro

/** All interactions take place within a single (implied) transaction. */
trait Service {
  def commit(): Unit
  def rollback(): Unit

  def getBalance(account: Long): Double
  def creditAccount(account: Long, amount: Double): Double
  def accounts(): Seq[Long]

  def debitAccount(account: Long, amount: Double) = {
    creditAccount(account, -amount)
  }

  def transfer(from: Long, to: Long, amount: Double) = {
    debitAccount(from, amount)
    creditAccount(to, amount)
  }

  def findOverdrawn() = for {
    account <- accounts()
    if getBalance(account) < 0.0
  } yield account

  def fineOverdrawn(fee: Double) = {
    findOverdrawn().foreach {
      debitAccount(_, fee)
    }
  }
}
