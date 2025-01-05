package oolong.mongo

import scala.quoted.Expr
import scala.quoted.Quotes

import oolong.bson.BsonEncoder
import oolong.bson.given
import org.mongodb.scala.bson.BsonValue

private[oolong] object BsonUtils {

  def extractLifted(expr: Expr[Any])(using q: Quotes): Expr[BsonValue] =
    import q.reflect.*
    expr match {
      case '{ $s: Long }    => '{ ${ s }.bson }
      case '{ $s: Int }     => '{ ${ s }.bson }
      case '{ $s: String }  => '{ ${ s }.bson }
      case '{ $s: Boolean } => '{ ${ s }.bson }
      case '{ $s: t } =>
        Expr.summon[BsonEncoder[t]] match {
          case Some(encoder) => '{ ${ encoder }.bson(${ s }) }
          case None if TypeRepr.of[t].isSingleton =>
            TypeRepr.of[t].widen.asType match {
              case '[tx] =>
                Expr.summon[BsonEncoder[tx]] match {
                  case Some(encoder) => '{ ${ encoder }.bson(${ s.asExprOf[tx] }) }
                  case _ => report.errorAndAbort(s"Didn't find bson encoder for type ${TypeRepr.of[t].widen.show}")
                }
            }
          case _ => report.errorAndAbort(s"Didn't find bson encoder for type ${TypeRepr.of[t].widen.show}")
        }
    }
}
