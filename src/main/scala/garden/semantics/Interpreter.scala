package garden.semantics

import garden.ir._

object ExprInterpreter {
  /** evaluating an expression with the initial store **/
  def eval(expr: Expr): Value = evalE(expr, σ0)

  /** ⇓e: evaluating an expression, given a store **/
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
  import ExprInterpreter.evalE

  /** evaluating a statement with the initial store **/
  def eval(stmt: Stmt): Result = evalS(stmt, σ0)

  /** ⇓s: evaluating a statement, given a store **/
  def evalS(stmt: Stmt, σ: Store): Result = stmt match {
    case Print(e)     ⇒ evalPrint(e, σ)
    case Block(stmts) ⇒ evalBlock(stmts, σ)
    case If0(e, s_t, s_f) ⇒ evalIf0(e, s_t, s_f, σ)
  }

  /** print **/
  def evalPrint(expr: Expr, σ: Store): Result = {
    // (1) evaluate the expression
    val v = evalE(expr, σ)

    // (2) print the result
    println(v)
  }
  
  /** blocks **/
  def evalBlock(stmts: Seq[Stmt], σ: Store): Result = 
      stmts foreach ((s: Stmt) ⇒ evalS(s, σ))
  
  /** if0 **/
  def evalIf0(condition: Expr, trueBranch: Stmt, falseBranch: Stmt, σ: Store) = {
    // (1) evaluate condition
    val conditionValue = evalE(condition, σ)
    
    // (2) based on condition's value, evaluate true or false branch
    if (conditionValue == 0)
      evalS(trueBranch, σ)
    else 
      evalS(falseBranch, σ)
  }
}
