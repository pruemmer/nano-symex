

/**
 * Extremely simple symbolic execution engine.
 */
class SymEx(encoder : ExprEncoder, spawnSMT : => SMT) {

  val smt = spawnSMT

  import encoder._
  import Program._
  import PType.PInt
  import smt._

  def shutdown = smt.shutdown

  smt.logCommands(false)

  def exec(p : Prog, variables : Seq[Var], depth : Int = Integer.MAX_VALUE) = {
    for (v@Var(name, PInt) <- variables)
      declareConst(name, IntType)
    val store =
      (for (v@Var(name, PInt) <- variables) yield (v -> name)).toMap

    execHelp(p, List(), depth)(store)

    reset
  }

  def execHelp(p : Prog, ops : List[Prog], depth : Int)
              (implicit store : SymbStore) : Unit = p match {

    case _ if ops.size > depth => ()

    case Skip => ()

    case Sequence(Skip, rest) =>
      execHelp(rest, ops, depth)

    case Sequence(Sequence(p1, p2), p3) =>
      execHelp(Sequence(p1, Sequence(p2, p3)), ops, depth)

    case Sequence(op@Assign(lhs : Var, rhs), rest) => {
      val newConst = freshConst(IntType)
      addAssertion("(= " + newConst + " " + encode(rhs) + ")")
      val newStore = store + (lhs -> newConst)
      execHelp(rest, op :: ops, depth)(newStore)
    }

    case Sequence(IfThenElse(cond, b1, b2), rest) => {
      val condStr = encode(cond)
      push
      addAssertion(condStr)
      val trueBranchSat = isSat
      if (trueBranchSat)
        execHelp(Sequence(b1, rest), ops, depth)
      pop
      push
      addAssertion("(not " + condStr + ")")
      if (!trueBranchSat || isSat)
        execHelp(Sequence(b2, rest), ops, depth)
      pop
    }

    case Sequence(w@While(cond, body), rest) =>
      execHelp(Sequence(IfThenElse(!cond, Skip, Sequence(body, w)), rest),
               ops, depth)

    case Sequence(a@Assert(cond), rest) => {
      push
      addAssertion(encode(!cond))
      if (isSat) {
        println("Found path leading to failing assertion:")
        for (op <- (a :: ops).reverse)
          println("  " + op)
      }
      pop
      execHelp(rest, ops, depth)
    }

    case p =>
      execHelp(Sequence(p, Skip), ops, depth)

  }

}


object SymExTest extends App {

  import ExampleProg._

  val symex = new SymEx(IntExprEncoder, new Z3SMT)

  symex.exec(p, List(a, b, x, y))

}

object SymExTest2 extends App {

  import ExampleProg2._

  val symex = new SymEx(IntExprEncoder, new Z3SMT)

  symex.exec(p, List(a, x), 200)

}
