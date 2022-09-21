package ru.tinkoff.oolong

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.quoted.*

import ru.tinkoff.oolong.Utils.AsRegexPattern

private[oolong] object Utils:

  def useWithinMacro(name: String) =
    scala.sys.error(s"`$name` should only be used within `compile` macro")

  object AsIterable {
    def unapply[T: Type](using q: Quotes)(
        expr: Expr[Iterable[T]]
    ): Option[Iterable[Expr[T]]] = {
      import q.reflect.*
      def rec(tree: Term): Option[Iterable[Expr[T]]] = tree match {
        case Repeated(elems, _) => Some(elems.map(x => x.asExprOf[T]))
        case Typed(e, _)        => rec(e)
        case Block(Nil, e)      => rec(e)
        case Apply(_, List(e))  => rec(e)
        case Inlined(_, Nil, e) => rec(e)
        case _                  => None
      }
      rec(expr.asTerm)
    }
  }

  object AsSome {
    def unapply[T: Type](expr: Expr[Option[T]])(using q: Quotes): Option[Expr[T]] = {
      import q.reflect.*
      expr.asTerm match
        case Apply(TypeApply(Select(Ident("Some"), "apply"), _), List(e)) => Some(e.asExprOf[T])
        case _                                                            => None
    }
  }

  object AnonfunBlock {
    def unapply(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[(String, quotes.reflect.Term)] = {
      import quotes.reflect.*
      term match {
        case Lambda(ValDef(paramName, _, _) :: Nil, rhs) =>
          Some((paramName, rhs))
        case _ => None
      }
    }
  }

  object InlinedSubquery {
    def unapply(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[quotes.reflect.Term] = {
      import quotes.reflect.*
      term match {
        case Inlined(_, _, expansion) => unapply(expansion)
        case Typed(term, _)           => Some(term)
        case _                        => None
      }
    }

    def unapply(expr: Expr[Any])(using quotes: Quotes): Option[quotes.reflect.Term] = {
      import quotes.reflect.*
      unapply(expr.asTerm)
    }
  }

  object AsTerm {
    def unapply(using quotes: Quotes)(
        expr: Expr[Any]
    ): Option[quotes.reflect.Term] = {
      import quotes.reflect.*
      Some(expr.asTerm)
    }
  }

  object PropSelector {
    private def parse(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[(String, List[String])] = {
      import quotes.reflect.*

      def loop(current: Term, acc: List[String]): Option[(String, List[String])] =
        current match {
          case Select(next, field) =>
            loop(next, field :: acc)
          case Apply(TypeApply(Ident("!!"), _), List(next)) =>
            loop(next, acc)
          case Ident(name) =>
            Some((name, acc))

          // Ident() can be inlined if queries are composed via "inline def"
          case Inlined(_, _, expansion) =>
            loop(expansion, acc)

          case _ =>
            None
        }

      loop(term, Nil)
    }

    def unapply(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[(String, List[String])] = parse(term)

    def unapply(using quotes: Quotes)(
        expr: Expr[_]
    ): Option[(String, List[String])] = {
      import quotes.reflect.*
      parse(expr.asTerm)
    }
  }

  object AsRegexPattern:
    def unapply(using quotes: Quotes)(
        expr: Expr[Any]
    ): Option[Pattern] =
      import quotes.reflect.*

      val mapNameToFlag: String => Int = {
        case "UNIX_LINES"              => Pattern.UNIX_LINES
        case "CASE_INSENSITIVE"        => Pattern.CASE_INSENSITIVE
        case "COMMENTS"                => Pattern.COMMENTS
        case "MULTILINE"               => Pattern.MULTILINE
        case "LITERAL"                 => Pattern.LITERAL
        case "DOTALL"                  => Pattern.DOTALL
        case "UNICODE_CASE"            => Pattern.UNICODE_CASE
        case "UNICODE_CHARACTER_CLASS" => Pattern.UNICODE_CHARACTER_CLASS
        case _                         => report.errorAndAbort("Unknown regex flag")
      }

      def parseFlags(term: Term): List[Int] =

        @tailrec
        def parseFlagsRec(term: Term)(flags: List[Int]): List[Int] =
          term match
            case Apply(
                  term,
                  List(Select(Ident("Pattern"), flag))
                ) =>
              parseFlagsRec(term)(flags :+ mapNameToFlag(flag))
            case Select(term, "|")              => parseFlagsRec(term)(flags)
            case Select(Ident("Pattern"), flag) => flags :+ mapNameToFlag(flag)

        parseFlagsRec(term)(List.empty)

      def rec(term: Term): Option[Pattern] = term match
        case Typed(term, _) => rec(term)
        case Apply(
              Select(Ident("Pattern"), "compile"),
              List(
                Inlined(_, _, Typed(Literal(StringConstant(pattern)), _)),
                Literal(IntConstant(flags))
              )
            ) =>
          Some(Pattern.compile(pattern, flags))
        case Apply(Select(Ident("Pattern"), "compile"), List(Literal(StringConstant(pattern)))) =>
          Some(Pattern.compile(pattern))
        case Apply(
              Select(Ident("Pattern"), "compile"),
              List(
                Literal(StringConstant(pattern)),
                patternFlags
              )
            ) =>
          val flags = parseFlags(patternFlags)
          if (flags.isEmpty) Some(Pattern.compile(pattern))
          else Some(Pattern.compile(pattern, flags.reduce(_ | _)))
        case _ => None

      rec(expr.asTerm)

  object PatternInstance:
    given FromExpr[Pattern] = new FromExpr[Pattern]:
      def unapply(expr: Expr[Pattern])(using q: Quotes): Option[Pattern] =
        import q.reflect.*
        AsRegexPattern.unapply(expr)

  extension [A](sq: Seq[A]) {
    def pforall(pf: PartialFunction[A, Boolean]): Boolean = sq.forall(pf.applyOrElse(_, _ => false))
  }
