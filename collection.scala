import java.util.function._

implicit class StringMoreOps(val s: String) extends AnyVal {
  def isBlank: Boolean = s == null || s.isEmpty || s.trim.isEmpty
  def orElse(other: String): String = if (isBlank) other else s
  def toOption: Option[String] = Option.when(!isBlank)(s.trim)
}

implicit class LazyLog(val log: LoggingAdapter) extends AnyVal {
  def debugLazy(message: => String): Unit = {
    if (log.isDebugEnabled) {
      log.debug(message)
    }
  }
}

implicit class OptionMoreOps[A](val o: Option[A]) extends AnyVal {
  def mapOrElse[B](mappingFunction: A => B, otherwise: => B): B =
    if (o.isDefined) mappingFunction(o.get) else otherwise

  def mapToIntOrElse(
      mappingFunction: ToIntFunction[A],
      otherwise: IntSupplier
  ): Int =
    if (o.isDefined) mappingFunction.applyAsInt(o.get) else otherwise.getAsInt

  def mapToIntOrElse(mappingFunction: ToIntFunction[A], otherwise: Int): Int =
    if (o.isDefined) mappingFunction.applyAsInt(o.get) else otherwise

  def mapToDoubleOrElse(
      mappingFunction: ToDoubleFunction[A],
      otherwise: DoubleSupplier
  ): Double =
    if (o.isDefined) mappingFunction.applyAsDouble(o.get)
    else otherwise.getAsDouble

  def mapToDoubleOrElse(
      mappingFunction: ToDoubleFunction[A],
      otherwise: Double
  ): Double =
    if (o.isDefined) mappingFunction.applyAsDouble(o.get) else otherwise

  def mapContains[B](mappingFunction: A => B, elem: B): Boolean =
    o.isDefined && mappingFunction(o.get) == elem
}

implicit class MapMoreOps[K, V, M[K1, V1] <: collection.Map[K1, V1]](
    val m: M[K, V]
) extends AnyVal {
  def getMapOrElse[B](key: K, mappingFunction: V => B, otherwise: => B): B =
    if (m.contains(key)) mappingFunction(m(key)) else otherwise

  def getMapToIntOrElse(
      key: K,
      mappingFunction: ToIntFunction[V],
      otherwise: IntSupplier
  ): Int =
    if (m.contains(key)) mappingFunction.applyAsInt(m(key))
    else otherwise.getAsInt
}
