package oolong

import java.util.regex.Pattern
import scala.compiletime.asMatchable
import scala.quoted.Expr
import scala.util.Try

sealed private[oolong] trait QExpr

private[oolong] object QExpr {

  case class Gte(x: QExpr, y: QExpr) extends QExpr

  case class Lte(x: QExpr, y: QExpr) extends QExpr

  case class Gt(x: QExpr, y: QExpr) extends QExpr

  case class Lt(x: QExpr, y: QExpr) extends QExpr

  case class Eq(x: QExpr, y: QExpr) extends QExpr

  case class Ne(x: QExpr, y: QExpr) extends QExpr

  case class Not(x: QExpr) extends QExpr

  case class In(x: QExpr, y: List[QExpr] | QExpr) extends QExpr

  case class Nin(x: QExpr, y: List[QExpr] | QExpr) extends QExpr

  case class And(children: List[QExpr]) extends QExpr

  case class Or(children: List[QExpr]) extends QExpr

  case class Prop(path: String) extends QExpr

  case class Constant[T](s: T) extends QExpr

  case class ScalaCode(code: Expr[Any]) extends QExpr

  case class Collection(s: Iterable[QExpr] | QExpr) extends QExpr

  case class ScalaCodeIterable(code: Expr[Iterable[Any]]) extends QExpr

  case class Subquery(code: Expr[Any]) extends QExpr

  case class Exists(x: QExpr, y: QExpr) extends QExpr

  case class Size(x: QExpr, y: QExpr) extends QExpr

  case class Regex(x: QExpr, pattern: Expr[Pattern]) extends QExpr

  case class TypeCheck[T](x: QExpr, typeInfo: TypeInfo[T]) extends QExpr

  case class Mod(x: QExpr, divisor: QExpr, remainder: QExpr) extends QExpr

  case class ElemMatch(x: QExpr, y: QExpr) extends QExpr

  case class All(x: QExpr, y: List[QExpr] | QExpr) extends QExpr

  case class Projection(fields: Vector[String]) extends QExpr

  object NumericConstant {

    /**
     * Here we guarantee that Constant, Numeric & Class are consistent
     */
    transparent inline def unapply(qe: QExpr): Option[(Constant[_], Numeric[_], Class[_])] =
      qe match {
        case bc @ Constant(_: Byte)        => Some(bc, Numeric[Byte], classOf[Byte])
        case cc @ Constant(_: Int)         => Some(cc, Numeric[Int], classOf[Int])
        case lc @ Constant(_: Long)        => Some(lc, Numeric[Long], classOf[Long])
        case fc @ Constant(_: Float)       => Some(fc, Numeric[Float], classOf[Float])
        case dc @ Constant(_: Double)      => Some(dc, Numeric[Double], classOf[Double])
        case bic @ Constant(_: BigInt)     => Some(bic, Numeric[BigInt], classOf[BigInt])
        case bdc @ Constant(_: BigDecimal) => Some(bdc, Numeric[BigDecimal], classOf[BigDecimal])
        case _                             => None
      }
  }
}
