package oolong.dsl

import oolong.Utils.*

/**
 * Lift `a` into the target representation. The expression for `a` won't be analyzed by the macro, it'll be passed
 * through to the final stage. Use this for runtime values.
 */
def lift[A](a: A): A =
  useWithinMacro("lift")

/**
 * Wrapper for a subquery that's already in the target representation.
 */
def unchecked[A](subquery: Any): A =
  useWithinMacro("unchecked")

extension [A](a: Option[A])
  /**
   * Unwrap the underlying type out of option
   */
  def !! : A = useWithinMacro("!!")

sealed trait Updater[DocT] {
  def set[PropT, ValueT](selectProp: DocT => PropT, value: ValueT)(using
      PropT =:= ValueT
  ): Updater[DocT] =
    useWithinMacro("set")
  def setOpt[PropT, ValueT](selectProp: DocT => Option[PropT], value: ValueT)(using
      PropT =:= ValueT
  ): Updater[DocT] =
    useWithinMacro("setOpt")
  def inc[PropT, ValueT](selectProp: DocT => PropT, value: ValueT)(using
      PropT =:= ValueT,
  ): Updater[DocT] = useWithinMacro("inc")
  def mul[PropT, ValueT](selectProp: DocT => PropT, value: ValueT)(using
      PropT =:= ValueT,
  ): Updater[DocT] = useWithinMacro("mul")
  def min[PropT, ValueT](selectProp: DocT => PropT, value: ValueT)(using
      PropT =:= ValueT,
  ): Updater[DocT] = useWithinMacro("mul")
  def max[PropT, ValueT](selectProp: DocT => PropT, value: ValueT)(using
      PropT =:= ValueT,
  ): Updater[DocT] = useWithinMacro("mul")
  def unset[PropT](selectProp: DocT => PropT): Updater[DocT]                   = useWithinMacro("unset")
  def rename[PropT](selectProp: DocT => PropT, newName: String): Updater[DocT] = useWithinMacro("rename")
  def setOnInsert[PropT, ValueT](selectProp: DocT => PropT, value: ValueT)(using
      PropT =:= ValueT,
  ): Updater[DocT] = useWithinMacro("setOnInsert")

  def addToSet[PropT, ValueT](selectProp: DocT => Iterable[PropT], value: ValueT)(using
      PropT =:= ValueT
  ): Updater[DocT] = useWithinMacro("addToSet")

  def addToSetAll[PropT, ValueT](selectProp: DocT => Iterable[PropT], value: Iterable[ValueT])(using
      PropT =:= ValueT
  ): Updater[DocT] = useWithinMacro("addToSet")

  def popHead(selectProp: DocT => Iterable[?]): Updater[DocT] = useWithinMacro("popHead")
  def popLast(selectProp: DocT => Iterable[?]): Updater[DocT] = useWithinMacro("popLast")

  def pull[PropT, ValueT](selectProp: DocT => Iterable[PropT], input: PropT => Boolean)(using
      PropT =:= ValueT
  ): Updater[DocT] = useWithinMacro("pull")

  def pullAll[PropT, ValueT](selectProp: DocT => Iterable[PropT], input: Iterable[ValueT])(using
      PropT =:= ValueT
  ): Updater[DocT] = useWithinMacro("pullAll")
}
