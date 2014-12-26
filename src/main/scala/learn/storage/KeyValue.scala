package learn.storage

import scalaz.{Monoid, Lens}

case class KeyValue[T](branch: Map[String, KeyValue[T]] = Map.empty[String,KeyValue[T]], value: Option[T] = None) {
  def isEmpty = branch.isEmpty && value.isEmpty

  def nonEmpty = !isEmpty

  type KV = KeyValue[T]

  lazy val Empty: KV = KeyValue()

  val valueLens: Lens[KV, Option[T]] = Lens.lensu(
    (o, v) => o.copy(value = v),
    _.value
  )

  def branchLens(key: String): Lens[KV, KV] = Lens.lensu(
    (o, v) => o.copy(branch = if (v.isEmpty) o.branch - key else o.branch + (key -> v)),
    _.branch.getOrElse(key, Empty)
  )

  def selfLens: Lens[KV, KV] = Lens.lensu((o, v) => v, identity)

  def pathLens(keys: String) = KeyValue.tokenize(keys) match {
    case Nil => selfLens
    case ks =>
      ks.map(branchLens).reduce(_ >=> _)
  }

  def leaf(keys: String) = pathLens(keys) >=> valueLens

  def span(f: T => Boolean): (KV, KV) = {
    val (lb, rb) = branch.mapValues {
      _.span(f)
    }.foldLeft((Map.empty[String, KV], Map.empty[String, KV])) {
      case ((accLeft, accRight), (k, (left, right))) =>
        (if (left.isEmpty) accLeft else accLeft + (k -> left)) -> (if (right.isEmpty) accRight else accRight + (k -> right))
    }

    val lv = value.filter(f)
    val rv = value.filterNot(f)

    KeyValue(lb, lv) -> KeyValue(rb, rv)
  }

  def flatten: Map[String, T] =
    branch.map {
      case (k, v) =>
        v.flatten.map {
          case (kk, vv) =>
            (KeyValue.Sep + k + kk) -> vv
        }
    }.flatten.toMap ++ value.fold(Map.empty[String, T])(v => Map("" -> v))

  def flattenBranch(keys: String): Map[String,T] = keys.headOption match {
    case Some(KeyValue.Sep) => pathLens(keys).get(this).flatten.map(kv => kv.copy(_1 = keys + kv._1))
    case Some(_) => flattenBranch(KeyValue.SepStr + keys)
    case None => Map.empty
  }


  def apply(keys: String) = leaf(keys).get(this)

  def update(keys: String, v: T): KV = leaf(keys).set(this, Some(v))

  def add(keys: String, v: T)(implicit m: Monoid[T]) = leaf(keys).get(this).fold(Some(v))(vv => Some(m.append(v, vv)))

  def unset(keys: String): KV = leaf(keys).set(this, None)

  def removeBranch(keys: String) = pathLens(keys).set(this, Empty)
}

object KeyValue {

  val Sep = '/'
  val SepStr = Sep.toString

  def tokenize(keys: String): List[String] = keys.split(Sep).toList.filter(_.nonEmpty)
}