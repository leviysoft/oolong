package oolong

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.quoted.*

import oolong.UExpr.FieldUpdateExpr
import oolong.Utils.*
import oolong.bson.utils.Projection
import oolong.dsl.*

private[oolong] trait AstParser {
  def parseQExpr[Doc: Type](input: Expr[Doc => Boolean]): QExpr

  def parseProjectionQExpr[Doc: Type, Proj: Type]: QExpr

  def parseUExpr[Doc: Type](input: Expr[Updater[Doc] => Updater[Doc]]): UExpr
}

private[oolong] class DefaultAstParser(using quotes: Quotes) extends AstParser {
  import quotes.reflect.*

  override def parseProjectionQExpr[Doc: Type, Proj: Type]: QExpr =
    QExpr.Projection(projectionFields[Doc, Proj])

  private def projectionFields[Doc: Type, Proj: Type]: Vector[String] =
    import quotes.reflect.*
    val result = Projection.checkIfProjection[Doc, Proj]

    if (!result) report.errorAndAbort(s"${TypeRepr.of[Proj].show} is not a projection of ${TypeRepr.of[Doc].show}")
    Projection.projectionPaths[Doc, Proj]

  override def parseQExpr[Doc: Type](input: Expr[Doc => Boolean]): QExpr = {

    def makeConst[T](t: T) =
      QExpr.Constant(t)

    def constOrAbort[T](expr: Expr[T])(using FromExpr[T]): QExpr.Constant[T] =
      QExpr.Constant(extractConstant(expr.value))

    def parseIterable[T: Type](expr: Expr[Seq[T] | Set[T]]): List[QExpr] | QExpr =
      expr match {
        // AsIterable can ignore lift e.g. in following case: lift(List(List(Random.nextInt()))
        case '{ type t; lift($x: Seq[`t`] | Set[`t`]) } => QExpr.ScalaCodeIterable(x)
        case AsIterable(elems) =>
          elems.map {
            case '{ $t: Boolean } => constOrAbort(t)
            case '{ $t: Long }    => constOrAbort(t)
            case '{ $t: Int }     => constOrAbort(t)
            case '{ $t: Short }   => constOrAbort(t)
            case '{ $t: Byte }    => constOrAbort(t)
            case '{ $t: Double }  => constOrAbort(t)
            case '{ $t: Float }   => constOrAbort(t)
            case '{ $t: String }  => constOrAbort(t)
            case '{ $t: Char }    => constOrAbort(t)
            case '{ lift($x: t) } => QExpr.ScalaCode(x)
            case '{ type t; $x: Seq[`t`] | Set[`t`] } =>
              QExpr.Collection(parseIterable(x))
            case x => QExpr.ScalaCode(x) // are we sure we need this this case?
          }.toList
        case _ =>
          report.errorAndAbort("Unexpected expr while parsing AST: " + expr.asTerm.show(using Printer.TreeStructure))
      }

    val (paramName, rhs) = unwrapLambda(input.asTerm)

    def parse(input: Expr[_]): QExpr = input match {
      case '{ ($x: Boolean) || ($y: Boolean) } =>
        QExpr.Or(List(parse(x), parse(y)))

      case '{ ($x: Boolean) && ($y: Boolean) } =>
        QExpr.And(List(parse(x), parse(y)))

      case '{ ($x: Seq[_]).size == ($y: Int) } =>
        QExpr.Size(parse(x), parse(y))

      case '{ ($x: Seq[_]).length == ($y: Int) } =>
        QExpr.Size(parse(x), parse(y))

      case '{ type t; ($x: Seq[`t`]).exists($y: (`t` => Boolean)) } => // not text & where
        QExpr.ElemMatch(parse(x), parseQExpr(y))

      case '{ type t; ($y: Seq[`t`]).forall(s => ($x: Seq[`t`]).contains(s)) } =>
        QExpr.All(parse(x), parseIterable(y))

      case AsTerm(
            Apply(
              Select(Apply(Select(prop, "%"), List(divisor)), "=="),
              List(remainder)
            )
          ) =>
        checkNumericMod(prop, divisor, remainder)
        QExpr.Mod(parse(prop.asExpr), parse(divisor.asExpr), parse(remainder.asExpr))

      case AsTerm(Apply(Select(lhs, "<="), List(rhs))) =>
        QExpr.Lte(parse(lhs.asExpr), parse(rhs.asExpr))

      case AsTerm(Apply(Select(lhs, ">="), List(rhs))) =>
        QExpr.Gte(parse(lhs.asExpr), parse(rhs.asExpr))

      case AsTerm(Apply(Select(lhs, "=="), List(rhs))) =>
        QExpr.Eq(parse(lhs.asExpr), parse(rhs.asExpr))

      case AsTerm(Apply(Select(lhs, "<"), List(rhs))) =>
        QExpr.Lt(parse(lhs.asExpr), parse(rhs.asExpr))

      case AsTerm(Apply(Select(lhs, ">"), List(rhs))) =>
        QExpr.Gt(parse(lhs.asExpr), parse(rhs.asExpr))

      case AsTerm(Apply(Select(lhs, "!="), List(rhs))) =>
        QExpr.Ne(parse(lhs.asExpr), parse(rhs.asExpr))

      case AsTerm(Apply(TypeApply(Select(lhs @ Select(_, _), "contains"), _), List(rhs))) =>
        QExpr.Eq(parse(lhs.asExpr), parse(rhs.asExpr))

      case AsTerm(Select(Apply(TypeApply(Select(lhs @ Select(_, _), "contains"), _), List(rhs)), "unary_!")) =>
        QExpr.Ne(parse(lhs.asExpr), parse(rhs.asExpr))

      case '{ Pattern.matches($s, $x) } =>
        QExpr.Regex(parse(x), '{ Pattern.compile($s) })

      case '{ (${ s }: Pattern).matcher($x).matches() } =>
        QExpr.Regex(parse(x), s)

      case '{ ($x: String).matches($s) } =>
        QExpr.Regex(parse(x), '{ Pattern.compile($s) })

      case '{ type t; ($s: Seq[`t`]).contains($x: `t`) } =>
        QExpr.In(parse(x), parseIterable(s))

      case '{ type t; !($s: Seq[`t`]).contains($x: `t`) } =>
        QExpr.Nin(parse(x), parseIterable(s))

      case '{ type t; ($s: Set[`t`]).contains($x: `t`) } =>
        QExpr.In(parse(x), parseIterable(s))

      case '{ type t; !($s: Set[`t`]).contains($x: `t`) } =>
        QExpr.Nin(parse(x), parseIterable(s))

      case '{ ($x: Iterable[_]).isEmpty } =>
        QExpr.Size(parse(x), QExpr.Constant(0))

      case '{ ($x: Option[_]).isEmpty } =>
        QExpr.Exists(parse(x), QExpr.Constant(false))

      case '{ ($x: Option[_]).isDefined } =>
        QExpr.Exists(parse(x), QExpr.Constant(true))

      case PropSelector(name, path) if name == paramName =>
        QExpr.Prop(path.mkString("."))

      case '{ lift($x: t) } =>
        QExpr.ScalaCode(x)

      case '{ unchecked($x: t) } =>
        QExpr.Subquery(x)

      case '{ !($x: Boolean) } =>
        QExpr.Not(parse(x))

      case AsTerm(Literal(DoubleConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(FloatConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(LongConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(IntConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(ShortConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(ByteConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(StringConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(CharConstant(c))) =>
        makeConst(c)

      case AsTerm(Literal(BooleanConstant(c))) =>
        makeConst(c)

      case InlinedSubquery(term) =>
        parse(term.asExpr)

      case '{ type t; ($x: Any).isInstanceOf[`t`] } =>
        QExpr.TypeCheck(
          parse(x),
          new TypeInfo[t] {
            override val quotedType: quoted.Type[t] = Type.of[t]
          }
        )

      case _ =>
        report.errorAndAbort("Unexpected expr while parsing AST: " + input.show + s"; term: ${showTerm(input.asTerm)}")
    }

    parse(rhs.asExpr)
  }

  private val discrsOfMod = Vector("field", "divisor", "remainder")
  private def checkNumericMod(field: Term, divisor: Term, remainder: Term): Unit =
    def checkNumeric(term: Term, discr: String): Option[(TypeRepr, String)] =
      term.tpe.widen.asType match
        case '[t] if Expr.summon[Numeric[t]].isDefined => None
        case '[t]                                      => Some(TypeRepr.of[t] -> discr)

    val nonNumericFields: Vector[(TypeRepr, String)] =
      Vector(field, divisor, remainder)
        .lazyZip(discrsOfMod)
        .flatMap(checkNumeric)
    if nonNumericFields.nonEmpty then
      report.errorAndAbort(
        nonNumericFields
          .map { case (typ, discr) =>
            s"$discr of type ${typ.show} is not Numeric"
          }
          .mkString("\n")
      )

  override def parseUExpr[Doc: Type](input: Expr[Updater[Doc] => Updater[Doc]]): UExpr = {
    val (paramName, rhs) = unwrapLambda(input.asTerm)

    // format: off
    @tailrec
    def parseUpdater(
        expr: Expr[Updater[Doc]],
        acc: List[FieldUpdateExpr]
    ): UExpr.Update =
      expr match {
        case '{ type t; ($updater: Updater[Doc]).set[`t`, `t`]($selectProp, ($valueExpr:  `t`)) } =>
          val prop  = parsePropSelector(selectProp)
          val value = getValue(valueExpr)
          parseUpdater(updater, FieldUpdateExpr.Set(UExpr.Prop(prop), value) :: acc)

        case '{ type t; ($updater: Updater[Doc]).setOpt[`t`, `t`]($selectProp, ($valueExpr: `t`)) } =>
          val prop  = parsePropSelector(selectProp)
          val value = getValue(valueExpr)
          parseUpdater(updater, FieldUpdateExpr.Set(UExpr.Prop(prop), value) :: acc)

        case '{type t; ($updater: Updater[Doc]).unset[`t`]($selectProp)} =>
          val prop = parsePropSelector(selectProp)
          parseUpdater(updater, FieldUpdateExpr.Unset(UExpr.Prop(prop)) :: acc)

        case '{ type t; ($updater: Updater[Doc]).inc[`t`, `t`]($selectProp, ($valueExpr: `t`)) } =>
          Expr.summon[Numeric[t]] match {
            case Some(_) =>
              val prop  = parsePropSelector(selectProp)
              val value = getValue(valueExpr)
              parseUpdater(updater, FieldUpdateExpr.Inc(UExpr.Prop(prop), value) :: acc)
            case _ => report.errorAndAbort(s"Trying to $$inc field that is not numeric")
          }

        case '{ type t; ($updater: Updater[Doc]).mul[`t`, `t`]($selectProp, ($valueExpr: `t`)) } =>
          Expr.summon[Numeric[t]] match {
            case Some(_) =>
              val prop  = parsePropSelector(selectProp)
              val value = getValue(valueExpr)
              parseUpdater(updater, FieldUpdateExpr.Mul(UExpr.Prop(prop), value) :: acc)
            case _ => report.errorAndAbort(s"Trying to $$mul field that is not numeric")
          }

        case '{ type t; ($updater: Updater[Doc]).min[`t`, `t`]($selectProp, ($valueExpr: `t`)) } =>
          val prop  = parsePropSelector(selectProp)
          val value = getValue(valueExpr)
          parseUpdater(updater, FieldUpdateExpr.Min(UExpr.Prop(prop), value) :: acc)

        case '{ type t; ($updater: Updater[Doc]).max[`t`, `t`]($selectProp, ($valueExpr: `t`)) } =>
          val prop  = parsePropSelector(selectProp)
          val value = getValue(valueExpr)
          parseUpdater(updater, FieldUpdateExpr.Max(UExpr.Prop(prop), value) :: acc)

        case '{ type t; ($updater: Updater[Doc]).rename[`t`]($selectProp, ($valueExpr: String)) } =>
          val prop  = parsePropSelector(selectProp)
          val value = getValue(valueExpr)
          parseUpdater(updater, FieldUpdateExpr.Rename(UExpr.Prop(prop), value) :: acc)

        case '{ type t; ($updater: Updater[Doc]).setOnInsert[`t`, `t`]($selectProp, ($valueExpr: `t`)) } =>
          val prop  = parsePropSelector(selectProp)
          val value = getValue(valueExpr)
          parseUpdater(updater, FieldUpdateExpr.SetOnInsert(UExpr.Prop(prop), value) :: acc)

        case '{ $updater: Updater[Doc] } =>
          updater match {
            case AsTerm(Ident(name)) if name == paramName =>
              UExpr.Update(acc)
            case _ =>
              report.errorAndAbort(
                s"(possibly a bug) Unexpected updater while parsing an 'update': ${updater.show}"
              )
          }

        case _ =>
          report.errorAndAbort(s"Unexpected expr while parsing an 'update': ${expr.show}")
      }

    parseUpdater(rhs.asExprOf[Updater[Doc]], Nil)
  }
  //format: on

  private def showTerm(term: Term) =
    term.show(using Printer.TreeStructure)

  private def extractConstant[T](valueOpt: Option[T]): T =
    valueOpt.getOrElse(report.errorAndAbort("Use `lift` for runtime values"))

  private def getConstant[T: Type](expr: Expr[T]): UExpr = {
    def constOrAbort(valueOpt: Option[T]) = UExpr.Constant(extractConstant(valueOpt))

    expr match {
      case '{ ${ t }: Long }    => constOrAbort(t.value)
      case '{ ${ t }: Int }     => constOrAbort(t.value)
      case '{ ${ t }: Short }   => constOrAbort(t.value)
      case '{ ${ t }: Byte }    => constOrAbort(t.value)
      case '{ ${ t }: Double }  => constOrAbort(t.value)
      case '{ ${ t }: Float }   => constOrAbort(t.value)
      case '{ ${ t }: String }  => constOrAbort(t.value)
      case '{ ${ t }: Char }    => constOrAbort(t.value)
      case '{ ${ t }: Boolean } => constOrAbort(t.value)
      case _ =>
        report.errorAndAbort("Unsupported constant type, consider using `lift`")
    }
  }

  private def getValue(expr: Expr[Any]): UExpr =
    expr match
      case '{ lift($x) }     => UExpr.ScalaCode(x)
      case '{ $constant: t } => getConstant(constant)

  private def unwrapLambda(input: Term): (String, Term) =
    input match {
      case Inlined(_, _, expansion) =>
        unwrapLambda(expansion)
      case AnonfunBlock(paramName, body) =>
        (paramName, body)
      case _ =>
        report.errorAndAbort(s"Expected a lambda, got ${showTerm(input)}")
    }

  private def parsePropSelector[DocT, PropT](select: Expr[DocT => PropT]): String = {
    def extract(lambda: Term): List[String] =
      lambda match {
        case Inlined(_, _, inlined) =>
          extract(inlined)
        case Lambda(_, PropSelector(_, path)) =>
          path
        case term =>
          report.errorAndAbort(s"Expected lambda, got ${showTerm(term)}")
      }

    extract(select.asTerm).mkString(".")
  }

}
