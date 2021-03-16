/**
 * Extremely simple dynamic symbolic execution engine.
 */
class DSE(encoder : ExprEncoder, spawnSMT : => SMT) {

  val smt = spawnSMT

  import encoder._
  import Program._
  import PType.{PInt, PArray}
  import smt._
  import SMT._
  import scala.collection.mutable.Queue

  val valQueue = Queue[(Valuation, Int)]()

  def shutdown = smt.shutdown

  smt.logCommands(false)

  def exec(p : Prog, variables : Seq[Var], depth : Int = Integer.MAX_VALUE) = {
      
    val firsttest =
      (for (v@Var(name, PInt) <- variables) yield (v -> BigInt(0))).toMap
    val initstore =
      (for (v@Var(name, PInt) <- variables) yield (v -> name)).toMap

    valQueue.clear

    valQueue.enqueue((firsttest,0))

    println("First test case" + firsttest)

    while (!valQueue.isEmpty) {
      val (test,bl) = valQueue.dequeue
      println("now starting" + test + "level" + bl)
      for (v@Var(name, PInt) <- variables)
        declareConst(name, IntType)
      execHelp(p, variables, test, bl, 0)(initstore)
      reset
    }

  }

  def execHelp(p : Prog, variables : Seq[Var],
               valuation : Valuation,
               bl : Int, curl: Int)
              (implicit store : SymbStore) : Unit = p match {

    case Skip => ()

    case Sequence(Skip, rest) =>
      execHelp(rest, variables, valuation, bl, curl)

    case Sequence(Sequence(p1, p2), p3) =>
      execHelp(Sequence(p1, Sequence(p2, p3)), variables, valuation, bl, curl)

    case Sequence(op@Assign(lhs : Var, rhs), rest) => {
      val newConst = freshConst(IntType)
      val newValuation = valuation + (lhs -> eval(rhs,valuation))
      addAssertion("(= " + newConst + " " + encode(rhs) + ")")
      val newStore = store + (lhs -> newConst)
      execHelp(rest, variables, newValuation, bl, curl)(newStore)
    }

    case Sequence(IfThenElse(cond, b1, b2), rest) => {
      println("level " + curl)
      if (eval(cond, valuation)) {
        if (bl <= curl) {
  	  push
          addAssertion(encode(Not(cond))) ;
          if (isSat) {
            val newtest =
              (for (v@Var(name, PInt) <- variables)
               yield (v -> getSatValue(name))).toMap
            println("new test case" + newtest)
            valQueue.enqueue((newtest,curl+1))
          }
          pop
	}
	addAssertion(encode(cond))
        execHelp(Sequence(b1, rest), variables, valuation, bl, curl+1)
      } else {
          println("level " + curl + " orig " + bl)
          if (bl <= curl) {
            push
            addAssertion(encode(cond))
            if (isSat) {
              val newtest =
                (for (v@Var(name, PInt) <- variables)
                 yield (v -> getSatValue(name))).toMap
              println("new test case" + newtest)
              valQueue.enqueue((newtest,curl+1))
            }
	    pop
	  }
          addAssertion(encode(Not(cond)))
          execHelp(Sequence(b2, rest), variables, valuation, bl, curl+1)
      }
    }

    case Sequence(w@While(cond, body), rest) =>
      execHelp(Sequence(IfThenElse(!cond, Skip, Sequence(body, w)), rest),
               variables, valuation, bl, curl)

    case Sequence(a@Assert(cond), rest) => {    // Still to be fixed
      push
      addAssertion(encode(!cond))
      if (isSat) {
        println("Found testcase leading to failing assertion:")
        val failtest =
          (for (v@Var(name, PInt) <- variables)
           yield (v -> getSatValue(name))).toMap
        println(failtest)
      }
      pop
      execHelp(rest, variables, valuation, bl, curl)
    }

    case p =>
      execHelp(Sequence(p, Skip), variables, valuation, bl, curl)

  }

}


object DSETest extends App {

  import ExampleProg._

  val dse = new DSE(IntExprEncoder, new Z3SMT)

  dse.exec(p, List(a, b, x, y))

}

object DSETest2 extends App {

  import ExampleProg2._

  val dse = new DSE(IntExprEncoder, new Z3SMT)

  dse.exec(p, List(a, x), 200)

}

object DSETest3 extends App {

  import ExampleProg3._

  val dse = new DSE(IntExprEncoder, new Z3SMT)

  dse.exec(p, List(a, b))

}
