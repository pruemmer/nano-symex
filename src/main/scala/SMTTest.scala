

object SMTTest extends App {

  for (smt <- List(new PrincessSMT, new Z3SMT))
  try {
    import smt._
    println("Testing SMT solver " + name + " ...")

    declareConst("x", "Int")
    declareConst("y", "Int")

    addAssertion("(> x y)")
    println(isSat)

  } finally {
    smt.shutdown
  }

}
