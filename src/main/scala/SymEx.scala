

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

    execHelp(p, List(), depth)(store, Map())

    reset
  }

  def execHelp(p : Prog, ops : List[Prog], depth : Int)
              (implicit store : SymbStore, arrays : SymbArStore) : Unit = p match {

    case _ if ops.size > depth => ()

    case Skip => ()

    case Sequence(Skip, rest) =>
      execHelp(rest, ops, depth)

    case Sequence(Sequence(p1, p2), p3) =>
      execHelp(Sequence(p1, Sequence(p2, p3)), ops, depth)

    case Sequence(op@Assign(lhs : Var, rhs : ArrayElem), rest) => {
      val indexVar = freshConst(IntType)
      //println("push " + indexVar)
      push
      addAssertion("(= " + indexVar + " " + encode(rhs.index) + ")")
      var repeat = 0
      while(isSat) {
        val index = getSatValue(indexVar)
	//println(repeat + " " + indexVar + " " + index + encode(rhs.index))
	//println("pop " + indexVar)
	val oldarray = if(arrays.contains(rhs.name)) arrays(rhs.name) else Map[BigInt, String]()
	val newConst = freshConst(IntType)
	val newarray = if(oldarray.contains(index)) oldarray else oldarray + (index -> newConst)
	val newStore = store + (lhs -> newarray(index))
	val newarrays = arrays + (rhs.name -> newarray)
        execHelp(rest, op :: ops, depth)(newStore, newarrays)
	addAssertion("(not (= " + indexVar + " " + index + "))")
	//println("(not (= " + indexVar + " " + index + "))")
	//println("push " + indexVar)
	repeat = repeat + 1
      }
      //println("pop " + indexVar)
      pop
    }

    case Sequence(op@Assign(lhs : Var, rhs), rest) => {
      val newConst = freshConst(IntType)
      addAssertion("(= " + newConst + " " + encode(rhs) + ")")
      val newStore = store + (lhs -> newConst)
      execHelp(rest, op :: ops, depth)(newStore, arrays)
    }

    case Sequence(op@Assign(lhs : ArrayElem, rhs), rest) => {
      var indexVar = freshConst(IntType)
      push
      addAssertion("(= " + indexVar + " " + encode(lhs.index) + ")")
      var repeat = 0
      while(isSat) {
        val index = getSatValue(indexVar)
	val oldarray = if(arrays.contains(lhs.name)) arrays(lhs.name) else Map[BigInt, String]()
	val newConst = freshConst(IntType)
	val newarray = oldarray + (index -> newConst)
	val newarrays = arrays + (lhs.name -> newarray)
	push
	addAssertion("(= " + newConst + " " + encode(rhs) + ")")
        execHelp(rest, op :: ops, depth)(store, newarrays)
	pop
	addAssertion("(not (= " + indexVar + " " + index + "))")
	repeat = repeat + 1
      }
      pop
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

object SymExArrayTest extends App {

  import InsSort._

  val symex = new SymEx(IntExprEncoder, new Z3SMT)

  symex.exec(p, List(i, j, x, y, len), 20)

}

