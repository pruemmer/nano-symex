
object Program {

  /**
   * Integer-valued expressions.
   */

  abstract sealed class Expr {
    def +(that : Expr)   = Plus(this, that)
    def -(that : Expr)   = Plus(this, that * (-1))
    def unary_-          = this * (-1)
    def *(that : Expr)   = Times(this, that)
    def <=(that : Expr)  = Leq(this, that)
    def >=(that : Expr)  = Leq(that, this)
    def <(that : Expr)   = Leq(this + 1, that)
    def >(that : Expr)   = Leq(that + 1, this)
    def ===(that : Expr) = Eq(that, this)
    def =/=(that : Expr) = !Eq(that, this)
  }

  case class Var     (name : String,
                      ptype : PType.Value = PType.PInt)   extends Expr
  case class IntConst(value : BigInt)                     extends Expr
  case class Plus    (left : Expr, right : Expr)          extends Expr
  case class Times   (left : Expr, right : Expr)          extends Expr

  implicit def int2Expr(v : Int) : Expr = IntConst(v)

  object PType extends Enumeration {
    // TODO: PArray is not handled yet
    val PInt, PArray = Value
  }

  //////////////////////////////////////////////////////////////////////////////

  /**
   * Boolean expressions / predicates.
   */

  abstract sealed class BExpr {
    def &(that : BExpr) = And(this, that)
    def |(that : BExpr) = Or (this, that)
    def unary_!         = Not(this)
  }

  case class Eq  (left : Expr, right : Expr)     extends BExpr
  case class Leq (left : Expr, right : Expr)     extends BExpr

  case class Not (sub : BExpr)                   extends BExpr
  case class And (left : BExpr, right : BExpr)   extends BExpr
  case class Or  (left : BExpr, right : BExpr)   extends BExpr

  //////////////////////////////////////////////////////////////////////////////

  /**
   * While-programs.
   */

  abstract sealed class Prog

  case object Skip                                           extends Prog

  case class  Assign    (v : Expr, rhs : Expr)               extends Prog
  case class  Assert    (cond : BExpr)                       extends Prog

  case class  Sequence  (left : Prog, right : Prog)          extends Prog
  case class  IfThenElse(cond : BExpr, b1 : Prog, b2 : Prog) extends Prog
  case class  While     (cond : BExpr, body : Prog)          extends Prog

  def Prog(stmts : Prog*) : Prog =
    if (stmts.isEmpty)
      Skip
    else
      stmts reduceRight (Sequence(_, _))

  def If(cond : BExpr)(branch : Prog*) =
    IfThenElse(cond, Prog(branch : _*), Skip)

  def While(cond : BExpr)(body : Prog*) : Prog =
    While(cond, Prog(body : _*))

  implicit def var2LHS(v : Var) = new AnyRef {
    def :=(that : Expr) = Assign(v, that)
  }

  implicit def ite2RichIte(p : IfThenElse) = new AnyRef {
    def Else(branch : Prog*) =
      IfThenElse(p.cond, p.b1, Prog(branch : _*))
  }

}

object ExampleExpr {

  import Program._

  val x = Var("x")
  val y = Var("y")

  val f = 0 <= x & x <= 10 & x + y === 42
  val g = y <= 20

}

object ExprTest extends App {

  import Program._
  import IntExprEncoder._
  import ExampleExpr._

  println("f: " + f)
  println("g: " + g)

  for (smt <- List(new Z3SMT, new PrincessSMT)) try {
    import smt._
    println("Testing SMT solver " + name + " ...")

    implicit val store : SymbStore = Map(x -> "x", y -> "y")

    for (encoder <- List(IntExprEncoder, new BVExprEncoder (32))) {
      import encoder._

      println("  sort " + IntType)

      push
      declareConst("x", IntType)
      declareConst("y", IntType)

      addAssertion(encode(f))
      println("    f is sat: " + isSat)

      addAssertion(encode(g))
      println("    f & g is sat: " + isSat)
      pop
    }
  } finally {
    smt.shutdown
  }

}

object ExampleProg {

  import Program._

  val a = Var("a")
  val b = Var("b")
  val x = Var("x")
  val y = Var("y")

  val p = Prog(
    x := 1,
    y := 0,
    If (a =/= 0) (
      y := 3+x,
      If (b === 0) (
        x := 2*(a+b)
      )
    ),
    Assert(x-y =/= 0)
  )

}

object ExampleProg2 {

  import Program._

  val a = Var("a")
  val x = Var("x")

  val p = Prog(
    x := 0,
    While (x =/= a) (
      x := x + 1
    ),
    Assert(x === a)
  )

}

object ExampleProg3 {

  import Program._

  val a = Var("a")
  val b = Var("b")

  val p = Prog(
    a := a + 1,
    b := (a + b) - 3, 
    If (a === b) (
        Assert(a =/= 0)
    )
  )
}

object ProgTest extends App {

  println(ExampleProg.p)

}
