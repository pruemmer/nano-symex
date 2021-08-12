

object SMTTest extends App {

  for (smt <- List(new Z3SMT))
  try {
    import smt._
    println("Testing SMT solver " + name + " ...")

    declareConst("const_232", "Int")
    declareConst("y", "Int")

    addAssertion("(= const_232 41)")
    addAssertion("(> const_232 y)")
    println(isSat)
    if(isSat) println(getSatValue("const_232"))

  } finally {
    smt.shutdown
  }

}
