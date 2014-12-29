package learn.storage

import scalaz.{Monoid, Lens}

final case class KeyValue[T](branch: Map[String, KeyValue[T]] = Map.empty[String,KeyValue[T]], value: Option[T] = None) {
  /**
   * Checks if this store contains neither value nor subbranch
   * @return
   */
  def isEmpty = branch.isEmpty && value.isEmpty

  /**
   * Checks if this store contains either value or subbranch
   * @return
   */
  def nonEmpty = !isEmpty

  /**
   * Shortcut for this type
   */
  type KV = KeyValue[T]

  /**
   * Empty instance for applicative builds
   */
  lazy val Empty: KV = KeyValue()

  /**
   * Lens for value object of concrete store
   */
  private val valueLens: Lens[KV, Option[T]] = Lens.lensu(
    (o, v) => o.copy(value = v),
    _.value
  )

  /**
   * Lens of subbranch store for key, always returns value (either existent or Empty)
   * @param key key
   * @return
   */
  private def branchLens(key: String): Lens[KV, KV] = Lens.lensu(
    (o, v) => o.copy(branch = if (v.isEmpty) o.branch - key else o.branch + (key -> v)),
    _.branch.getOrElse(key, Empty)
  )

  /**
   * Lens that returns this object
   * @return
   */
  private def selfLens: Lens[KV, KV] = Lens.lensu((o, v) => v, identity)

  /**
   * Lens for subbranch store. Returns self for empty keys
   * @param keys branch path
   * @return
   */
  private def pathLens(keys: String) = KeyValue.tokenize(keys) match {
    case Nil => selfLens
    case ks =>
      ks.map(branchLens).reduce(_ >=> _)
  }

  /**
   * Lens for value of subbranch
   * @param keys branch path
   * @return
   */
  private def leaf(keys: String) = pathLens(keys) >=> valueLens

  /**
   * Splits this KV tree into a pair basing on predicate: returning (true, false)
   * @param f predicate
   * @return
   */
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

  /**
   * Flattens the object into a map of full keys and values
   * @return
   */
  def flatten: Map[String, T] =
    branch.map {
      case (k, v) =>
        v.flatten.map {
          case (kk, vv) =>
            (KeyValue.Sep + k + kk) -> vv
        }
    }.flatten.toMap ++ value.fold(Map.empty[String, T])(v => Map("" -> v))

  /**
   * Flattens a subbranch into a map of full keys and values
   * @param keys
   * @return
   */
  def flattenBranch(keys: String): Map[String,T] = keys.headOption match {
    case Some(KeyValue.Sep) => pathLens(keys).get(this).flatten.map(kv => kv.copy(_1 = keys + kv._1))
    case Some(_) => flattenBranch(KeyValue.SepStr + keys)
    case None => Map.empty
  }


  /**
   * Returns value by key
   * @param keys value path
   * @return
   */
  def apply(keys: String): Option[T] = leaf(keys).get(this)

  /**
   * Returns a store with updated value
   * @param keys branch path
   * @param v value
   * @return
   */
  def update(keys: String, v: T): KV = leaf(keys).set(this, Some(v))

  /**
   * Adds a value to an existent one
   * @param keys value path
   * @param v value to add
   * @param m monoid to perform appending
   * @return
   */
  def add(keys: String, v: T)(implicit m: Monoid[T]): KV = {
    val l = leaf(keys)
    l.set(this, l.get(this).fold(Some(v))(vv => Some(m.append(vv, v))))
  }

  /**
   * Removes value from a branch
   * @param keys value path
   * @return
   */
  def unset(keys: String): KV = leaf(keys).set(this, None)

  /**
   * Removes branch with its value and subbranches
   * @param keys branch path
   * @return
   */
  def removeBranch(keys: String) = pathLens(keys).set(this, Empty)
}

object KeyValue {

  val Sep = '/'
  val SepStr = Sep.toString

  def tokenize(keys: String): List[String] = keys.split(Sep).toList.filter(_.nonEmpty)
}