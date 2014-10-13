package garden

import scala.language.implicitConversions

import garden.ir.FuncDef
import garden.ir.Var

package object semantics {
  /**
   * Domains
   */
  type Value = Int
  type Result = Unit
}