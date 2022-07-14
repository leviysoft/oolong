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

  def shrink(lhs: QExpr, rhs: QExpr): QExpr =
    (lhs, rhs) match {
      // Gt
      case (
            QExpr.Gt(QExpr.Prop(l1), QExpr.NumericConstant(lconst, lnum, lclass)),
            QExpr.Gt(QExpr.Prop(_), QExpr.NumericConstant(rconst, _, rclass))
          ) if lclass == rclass =>
        QExpr.Gt(QExpr.Prop(l1), QExpr.Constant(lnum.asInstanceOf[Numeric[Any]].max(lconst.s, rconst.s)))

      // Gte
      case (
            QExpr.Gte(QExpr.Prop(l1), QExpr.NumericConstant(lconst, lnum, lclass)),
            QExpr.Gte(QExpr.Prop(_), QExpr.NumericConstant(rconst, _, rclass))
          ) if lclass == rclass =>
        QExpr.Gte(QExpr.Prop(l1), QExpr.Constant(lnum.asInstanceOf[Numeric[Any]].max(lconst.s, rconst.s)))

      // Lt
      case (
            QExpr.Lt(QExpr.Prop(l1), QExpr.NumericConstant(lconst, lnum, lclass)),
            QExpr.Lt(QExpr.Prop(_), QExpr.NumericConstant(rconst, _, rclass))
          ) if lclass == rclass =>
        QExpr.Lt(QExpr.Prop(l1), QExpr.Constant(lnum.asInstanceOf[Numeric[Any]].min(lconst.s, rconst.s)))

      // Lte
      case (
            QExpr.Lte(QExpr.Prop(l1), QExpr.NumericConstant(lconst, lnum, lclass)),
            QExpr.Lte(QExpr.Prop(_), QExpr.NumericConstant(rconst, _, rclass))
          ) if lclass == rclass =>
        QExpr.Lte(QExpr.Prop(l1), QExpr.Constant(lnum.asInstanceOf[Numeric[Any]].min(lconst.s, rconst.s)))

      case _ => lhs
    }
}
