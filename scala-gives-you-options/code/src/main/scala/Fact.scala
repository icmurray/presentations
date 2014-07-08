object Fact {
  def fact(n: Int) = {

    @annotation.tailrec
    def factAcc(n: Int, acc: Int): Int = n match {
      case 0 => acc
      case 1 => acc
      case n => factAcc(n-1, acc*n)
    }

    factAcc(n, 1)
  }
}
