package oolong

private[oolong] object LogicalOptimizer {

  def optimize(ast: QExpr): QExpr = {

    // Example:
    //
    //         grandparent: And(And(x, y), z))
    //              /            \
    //    parent: And(x, y)      z
    //    /              \
    //   x                y
    //
    // should be transformed into
    //
    // grandparent: And(x, y, z)
    //     /      |      \
    //    x       y       z
    def flatten(grandparent: QExpr): QExpr = grandparent match {
      case QExpr.And(parents) =>
        val newParents = parents.flatMap {
          case QExpr.And(children) => children
          case parent              => List(parent)
        }
        QExpr.And(newParents)
      case QExpr.Or(parents) =>
        val newParents = parents.flatMap {
          case QExpr.Or(children) => children
          case parent             => List(parent)
        }
        QExpr.Or(newParents)
      case _ =>
        grandparent
    }

    ast match {
      case QExpr.ElemMatch(field, expr) => QExpr.ElemMatch(field, flatten(expr))
      case QExpr.And(children)          => flatten(QExpr.And(children.map(optimize)))
      case QExpr.Or(children)           => flatten(QExpr.Or(children.map(optimize)))
      case QExpr.Not(QExpr.Not(e))      => e
      case QExpr.Not(QExpr.Eq(l, r))    => QExpr.Ne(l, r)
      case _                            => ast
    }
  }
}
