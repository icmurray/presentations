trait Revisioning {
  type Data
  type Revisioned <: RevisionedLike

  def mkRevisioned(data: Data): Revisioned

  trait RevisionedLike {
    def revision: Long
    def data: Data
    def modify(f: Data => Data): Revisioned
  }
}
