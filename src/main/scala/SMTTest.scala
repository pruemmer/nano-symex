

object SMTTest extends App {

  for (smt <- List(new Z3SMT))
  try {
    import smt._
    println("Testing SMT solver " + name + " ...")

    declareConst("a", "Int")
    declareConst("y", "Int")

    addAssertion("(= a (- 5326))")
    addAssertion("(> a y)")
    println(isSat)
    if(isSat) println(getSatValue("a"))

  } finally {
    smt.shutdown
  }

}
