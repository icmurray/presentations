trait Monoid[T] {
  def zero: T
  def mappend(t1: T, t2: T): T
}

object Monoid {
  implicit val stringMonoid = new Monoid[String] {
    def zero = ""
    def mappend(s1: String, s2: String) = s1 + s2
  }

  implicit def optionMonoid[M: Monoid] = new Monoid[Option[M]] {
    def zero = None
    def mappend(m1: Option[M], m2: Option[M]) = (m1, m2) match {
      case (None, None)         => None
      case (None, Some(_))      => m2
      case (Some(_), None)      => m1
      case (Some(v1), Some(v2)) => {
        val monoid = implicitly[Monoid[M]]
        Some(monoid.mappend(v1, v2))
      }
    }
  }
}
