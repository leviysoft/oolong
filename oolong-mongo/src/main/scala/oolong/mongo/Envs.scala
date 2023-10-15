package oolong.mongo

object Envs:
  private val printAstKey = "oolong.print.ast"
  private[oolong] lazy val printAst =
    Option(System.getProperty(printAstKey)).orElse(sys.env.get(printAstKey)).getOrElse("false").toBoolean
