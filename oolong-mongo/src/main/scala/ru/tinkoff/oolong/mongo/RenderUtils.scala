package ru.tinkoff.oolong.mongo

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

import ru.tinkoff.oolong.Utils.AsIterable
import ru.tinkoff.oolong.Utils.AsSome
import ru.tinkoff.oolong.Utils.AsTerm

private[oolong] object RenderUtils {

  def renderCaseClass[A: Type](value: Expr[A])(using q: Quotes): String =
    import q.reflect.*

    def parseCaseClass[A: Type](value: Expr[A]): String = {
      val repr = TypeRepr.of[A].typeSymbol
      val paramsAndDefsOpt = value.asTerm.underlyingArgument.asExprOf[A] match
        case AsTerm(
              Apply(
                Select(Select(This(Some(_)), _), "apply"),
                list
              )
            ) =>
          Some(list -> Map.empty)
        case AsTerm(
              Apply(
                Select(Ident(_), "apply"),
                list
              )
            ) =>
          Some(list -> Map.empty)
        case AsTerm(
              Block(
                valDefList,
                Apply(
                  Select(Select(This(Some(_)), _), "apply"),
                  list
                )
              )
            ) =>
          val definitions = valDefList.collect { case ValDef(name, _, Some(definition)) => name -> definition }.toMap
          Some(list -> definitions)
        case AsTerm(
              Block(
                valDefList,
                Apply(
                  Select(Ident(_), "apply"),
                  list
                )
              )
            ) =>
          val definitions = valDefList.collect { case ValDef(name, _, Some(definition)) => name -> definition }.toMap
          Some(list -> definitions)
        case _ => None

      paramsAndDefsOpt match
        case Some(params -> definitions) =>
          val fields = repr.caseFields.map(_.name)
          val res = fields.zip(params).map { case (name, value) =>
            name + ":" + " " + parseConstant[A](value.asExpr)(definitions.map { case (a, b) => (a.toString, b) })
          }
          res.mkString("{", ", ", "}")
        case _ => "?"
    }

    def parseConstant[A: Type](expr: Expr[Any])(definitions: Map[String, q.reflect.Term]): String =
      expr match
        case AsTerm(Literal(DoubleConstant(c))) =>
          c.toString
        case AsTerm(Literal(FloatConstant(c))) =>
          c.toString
        case AsTerm(Literal(LongConstant(c))) =>
          c.toString
        case AsTerm(Literal(IntConstant(c))) =>
          c.toString
        case AsTerm(Literal(ShortConstant(c))) =>
          c.toString
        case AsTerm(Literal(ByteConstant(c))) =>
          c.toString
        case AsTerm(Literal(StringConstant(c))) =>
          s"\"$c\""
        case AsTerm(Literal(CharConstant(c))) =>
          c.toString
        case AsTerm(Literal(BooleanConstant(c))) =>
          c.toString
        case AsTerm(Select(_, name)) if name.contains("$lessinit$greater$default$") =>
          TypeRepr
            .of[A]
            .typeSymbol
            .companionClass
            .declaredMethod(name)
            .headOption
            .flatMap(_.tree.asInstanceOf[DefDef].rhs)
            .map(s => parseConstant[A](s.asExpr)(definitions))
            .getOrElse(name)
        case AsTerm(NamedArg(_, const))                    => parseConstant(const.asExpr)(definitions)
        case AsTerm(Select(Ident(cl), method))             => s"Function($cl.$method)"
        case AsTerm(Apply(Select(Ident(cl), method), Nil)) => s"Function($cl.$method())"
        case AsTerm(Ident(name)) =>
          if (name == "None") "null"
          else
            definitions
              .get(name)
              .map(s => parseConstant(s.asExpr)(definitions))
              .getOrElse(name)
        case AsIterable(list) => list.map(parseConstant(_)(definitions)).mkString("[", ", ", "]")
        case AsSome(value)    => parseConstant(value)(definitions)
        case '{ ${ x }: t }   => parseCaseClass[t](x)
        case _                => "?"

    parseCaseClass(value)
}
