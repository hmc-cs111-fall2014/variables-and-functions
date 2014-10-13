package garden.semantics

import garden.ir._

object ExprInterpreter {
  /** evaluating an expression with the initial store **/
  def eval(expr: Expr): Value = evalE(expr, σ0)

  /** ⇓e: evaluating an expression **/
  def evalE(expr: Expr, σ: Store): Value = expr match {
    case Num(i)            ⇒ i
    case x: Var            ⇒ σ(x)
    case Plus(left, right) ⇒ evalE(left, σ) + evalE(right, σ)
    case Sub(left, right)  ⇒ evalE(left, σ) - evalE(right, σ)
    case Mult(left, right) ⇒ evalE(left, σ) * evalE(right, σ)
    case Div(left, right)  ⇒ evalE(left, σ) / evalE(right, σ)
  }
}

object StmtInterpreter {
    /** ⇓s: evaluating a statement **/ 
  def eval(stmt: Stmt): Result = throw new NotImplementedError() 
}
