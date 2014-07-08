trait AbstractRevisioning extends Revisioning {

  def mkRevisioned(d: Data) = Revisioned(0L, d)

  case class Revisioned(revision: Long, data: Data) extends RevisionedLike {
    def modify(f: Data => Data) = Revisioned(revision+1, f(data))
  }
}
