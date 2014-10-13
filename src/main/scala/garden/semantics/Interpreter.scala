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
    case Print(e)         ⇒ evalPrint(e, σ)
    case Block(stmts)     ⇒ evalBlock(stmts, σ)
    case If0(e, s_t, s_f) ⇒ evalIf0(e, s_t, s_f, σ)
    case Set(x, e)        ⇒ evalAssign(x, e, σ)
    case Update(x, e)     ⇒ evalUpdate(x, e, σ)
  }

  /** print **/
  def evalPrint(expr: Expr, σ: Store): Result = {
    // (1) evaluate the expression
    val v = evalE(expr, σ)

    // (2) print the result
    println(v)
    
    σ // printing doesn't affect the store
  }
  
  /** blocks **/
  def evalBlock(stmts: Seq[Stmt], σ: Store): Result = 
    if (stmts.isEmpty)
      σ  // Empty blocks don't affect the environment or store
    else {
      // (1) evaluate 1st statement
      val σ1 = evalS(stmts.head, σ)  

      // (2) use result to evaluate rest of block
      evalS(Block(stmts.tail), σ1)  
    }
  /**
   * Note: we could also have written this code using a left-fold:
   *       
      def doNext(state: Store, stmt: Stmt): Result =  evalS(stmt, state)
        
      ( σ /: stmts )(doNext)
   * 
   */
  
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

  def evalAssign(variable: Var, expr: Expr, σ: Store): Result = {
    // error checking: make sure that var ∉ σ
    require(!(σ contains variable), 
            s"Redefinition of variable ${variable.name}")

    // (1) evaluate the left-hand side 
    val value = evalE(expr, σ)

    // (2) Bind the variable to the value in the environment
    val σ1 = σ + (variable → value)

    σ1
  }

  /** variable update **/
  def evalUpdate(variable: Var, expr: Expr, σ: Store): Result = {
    // error checking: make sure that var ∈ σ
    require(σ contains variable,
      s"""Cannot update non-existent variable ${variable.name}. 
    Try var ${variable.name} := ... ?""")

    // (1) evaluate the left-hand side 
    val value = evalE(expr, σ)

    // (2) Bind the address to the new value in the store
    val σ1 = σ + (variable → value)

    σ1
  }
}
