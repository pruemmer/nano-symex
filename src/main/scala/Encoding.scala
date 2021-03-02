
/**
 * Translation from ASTs to SMT expressions.
 */
abstract class ExprEncoder {

  import Program._

  val IntType : String

  type Valuation = Map[Var, BigInt]

  type SymbStore = Map[Var, String]

  def encode(expr : Expr)(implicit store : SymbStore) : String

  def encode(expr : BExpr)(implicit store : SymbStore) : String

  def eval(expr : Expr, valuation: Valuation) : BigInt

  def eval(expr : BExpr, valuation: Valuation) : Boolean

}

/**
 * Translation from ASTs to SMT expressions, mapping the program integer
 * type to unbounded (mathematical) integers.
 */
object IntExprEncoder extends ExprEncoder {

  import Program._

  val IntType : String = "Int"

  def encode(expr : Expr)
            (implicit store : SymbStore) : String = expr match {
    case v : Var     => store(v)
    case IntConst(v) => if (v >= 0) v.toString else ("(- " + -v + ")")
    case Plus(l, r)  => "(+ " + encode(l) + " " + encode(r) + ")"
    case Times(l, r) => "(* " + encode(l) + " " + encode(r) + ")"
  }

  def encode(expr : BExpr)
            (implicit store : SymbStore) : String = expr match {
    case Eq(l, r)    => "(= "   + encode(l) + " " + encode(r) + ")"
    case Leq(l, r)   => "(<= "  + encode(l) + " " + encode(r) + ")"
    case Not(s)      => "(not " + encode(s) + ")"
    case And(l, r)   => "(and " + encode(l) + " " + encode(r) + ")"
    case Or(l, r)    => "(or "  + encode(l) + " " + encode(r) + ")"
  }

  def eval(expr : Expr, valuation : Valuation) : BigInt = expr match {
    case v : Var     => valuation(v)
    case IntConst(v) => v
    case Plus(l, r)  => (eval(l,valuation) + eval(r,valuation))
    case Times(l, r) => (eval(l,valuation) * eval(r,valuation))
  }
  
  def eval(expr : BExpr, valuation : Valuation) : Boolean = expr match {
    case Eq(l, r)    => (eval(l,valuation) == eval(r,valuation))
    case Leq(l, r)   => (eval(l,valuation) <= eval(r,valuation))
    case Not(s)      => (!eval(s,valuation))
    case And(l, r)   => (eval(l,valuation) && eval(r,valuation))
    case Or(l, r)    => (eval(l,valuation) || eval(r,valuation))
  }

}

/**
 * Translation from ASTs to SMT expressions, mapping the program integer
 * type to signed bit-vectors of width <code>width</code>.
 */
class BVExprEncoder(width : Int) extends ExprEncoder {

  import Program._

  val IntType : String = "(_ BitVec " + width + ")"

  def encode(expr : Expr)
            (implicit store : SymbStore) : String = expr match {
    case v : Var     => store(v)
    case IntConst(v) =>
      if (v >= 0)
        "(_ bv" + v.toString + " " + width + ")"
      else
        "(bvneg (_ bv" + (-v).toString + " " + width + "))"
    case Plus(l, r)  => "(bvadd " + encode(l) + " " + encode(r) + ")"
    case Times(l, r) => "(bvmul " + encode(l) + " " + encode(r) + ")"
  }

  def encode(expr : BExpr)
            (implicit store : SymbStore) : String = expr match {
    case Eq(l, r)    => "(= "     + encode(l) + " " + encode(r) + ")"
    case Leq(l, r)   => "(bvsle " + encode(l) + " " + encode(r) + ")"
    case Not(s)      => "(not "   + encode(s) + ")"
    case And(l, r)   => "(and "   + encode(l) + " " + encode(r) + ")"
    case Or(l, r)    => "(or "    + encode(l) + " " + encode(r) + ")"
  }

  def eval(expr : Expr, valuation: Valuation) : BigInt = ???

  def eval(expr : BExpr, valuation: Valuation) : Boolean = ???

}

