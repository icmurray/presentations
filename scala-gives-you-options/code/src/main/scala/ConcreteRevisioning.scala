trait ConcreteRevisioning extends Revisioning {
  type Data = Person
}

object RevisionedPeople extends AbstractRevisioning with ConcreteRevisioning
