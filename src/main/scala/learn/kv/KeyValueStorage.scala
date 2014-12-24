package learn.kv

import learn.kv.Command._

import scala.collection.immutable.TreeMap

case class KeyValueStorage[T: ValueAppender](data: Map[String, KeyValueStorage[T]] = TreeMap.empty, value: Option[Value[T]] = None) {

  def isEmpty = data.isEmpty && value.isEmpty

  def nonEmpty = data.nonEmpty || value.nonEmpty

  def asOpt: Option[this.type] = if(isEmpty) None else Some(this)

  /**
   * Processes a mutating command
   * @param path inner path to process
   * @param cmd command to process
   * @return changed state
   */
  private def processCommand(path: List[String], cmd: KeyCommand[T]): KeyValueStorage[T] = path match {
    case p :: rest =>
      val s = data.getOrElse(p, KeyValueStorage[T]()).processCommand(rest, cmd)

      if (s.isEmpty) copy(data = data - p) else copy(data = data + (p -> s))

    case Nil =>
      cmd match {
        case Set(_, v, exp) =>
          copy(value = Some(Value(v, exp)))

        case Add(_, v, exp) =>
          value match {
            case None =>
              copy(value = Some(Value(implicitly[ValueAppender[T]].wrap(v), exp)))

            case Some(Value(vv, _)) =>
              copy(value = Some(Value(implicitly[ValueAppender[T]].append(vv, v), exp)))

          }

        case Delete(_) =>
          KeyValueStorage()
      }

  }

  /**
   * Processes a mutating command
   * @param cmd command to process
   * @return
   */
  def process(cmd: Command): KeyValueStorage[T] = cmd match {
    case c: KeyCommand[T] =>
      processCommand(c.path, c)
    case Expire(lt) =>
      expire(lt)
  }

  /**
   * Shortcut to set a key
   * @param key key
   * @param value value
   * @param expires expires
   * @return
   */
  def set(key: String, value: T, expires: Long) = process(Set(key, value, expires))

  /**
   * Shortcut to add to a key
   * @param key key
   * @param value value
   * @param expires expires
   * @return
   */
  def add(key: String, value: T, expires: Long) = process(Add(key, value, expires))

  /**
   * Shortcut to delete a key
   * @param key key
   * @return
   */
  def delete(key: String) = process(Delete(key))

  /**
   * Removes all values with expires field < lessThen argument
   * @param threshold to compare expired with
   * @return updated storage
   */
  def expire(threshold: Long): KeyValueStorage[T] = copy(
    data = data.map {
      case (k, v) => k -> v.expire(threshold)
    }.filter(_._2.nonEmpty),
    value = value.filter(_.expires >= threshold))

  /**
   * Removes all expired values,
   * @param threshold to compare expired with
   * @return
   */
  def expireList(threshold: Long): (Seq[(String, Value[T])], KeyValueStorage[T]) = {
    val (newValue, expired) = value match {
      case Some(v) if v.expires < threshold =>
        (None, Seq("" -> v))
      case _ =>
        (value, Seq.empty[(String,Value[T])])
    }

    val (dataExpired, newData) = {
      data.map {
        case (k, v) =>
          val inExpired = v.expireList(threshold)

          (inExpired._1.map{
            case (kk, vv) =>
              (KeyValueStorage.Separator + k + kk) -> vv
          },

          if(inExpired._2.isEmpty) None else Some(k -> inExpired._2))
      }.foldLeft((Seq.empty[(String,Value[T])], TreeMap.empty[String,KeyValueStorage[T]])) {
        case ((accExps, accData), (exps, updd)) =>
          (accExps ++ exps) -> updd.fold(accData)(accData + _)
      }
    }

    (expired ++ dataExpired, copy(value = newValue, data = newData))
  }

  /**
   * Removes all expired values
   * @param threshold to compare expired with
   * @return
   */
  def expireValues(threshold: Long): (Seq[(String,T)], KeyValueStorage[T]) = {
    val expired = expireList(threshold)
    (expired._1.map(kv => kv._1 -> kv._2.value), expired._2)
  }

  /**
   * Returns value by key, if it exists
   * @param path path
   * @return
   */
  private def get(path: List[String]): Option[Value[T]] = path match {
    case p :: rest =>
      data.get(p).flatMap(_.get(rest))

    case Nil =>
      value
  }

  /**
   * Returns value by key, if it exists
   * @param key id
   * @return
   */
  def get(key: String): Option[Value[T]] = get(KeyValueStorage.split(key))

  /**
   * Shortcut for value retrieval
   * @param key id
   * @return
   */
  def value(key: String): Option[T] = get(key).map(_.value)

  /**
   * Lists all nested keys and values
   * @return
   */
  def list(): Seq[(String, Value[T])] =
    data.map {
      case (k, v) =>
        v.list().map {
          case (kk, vv) => (KeyValueStorage.Separator + k + kk) -> vv
        }.toSeq
    }.flatten.toSeq ++ value.fold(Seq.empty[(String, Value[T])])(v => Seq("" -> v))

  /**
   * Returns keys and values for a branch
   * @param key id
   * @return
   */
  def list(key: String): Seq[(String, Value[T])] = list(KeyValueStorage.split(key))

  /**
   * Shortcut to get values without Value[T] wrapper
   * @param key id
   * @return
   */
  def values(key: String): Seq[(String,T)] = list(key).map(kv => kv._1 -> kv._2.value)

  /**
   * Lists all values
   * @return
   */
  def values(): Seq[(String,T)] = list().map(kv => kv._1 -> kv._2.value)

  /**
   * Returns keys and values for a branch
   * @param path path
   * @return
   */
  private def list(path: List[String]): Seq[(String, Value[T])] = path match {
    case p :: rest =>
      data.get(p).map(_.list(rest).map {
        case (kk, vv) => (KeyValueStorage.Separator + p + kk) -> vv
      }).getOrElse(Seq.empty)
    case Nil =>
      list()
  }

  /**
   * Lists all non-empty keys
   * @return
   */
  def keys(): Seq[String] =
    data.map {
      case (k, v) =>
        v.keys().map(KeyValueStorage.Separator + k + _).toSeq
    }.flatten.toSeq ++ value.fold(Seq.empty[String])(_ => Seq(""))

  /**
   * Lists all keys inside a branch
   * @param path path
   * @return
   */
  private def keys(path: List[String]): Seq[String] = path match {
    case p :: rest =>
      data.get(p).map(_.keys(rest).map(KeyValueStorage.Separator + p + _)).getOrElse(Seq.empty)
    case Nil =>
      keys()
  }

  /**
   * Lists all keys inside a branch
   * @param key id
   * @return
   */
  def keys(key: String): Seq[String] = keys(KeyValueStorage.split(key))
}

object KeyValueStorage {
  val Separator = '/'

  def split(id: String): List[String] = id.split('/').toList.filter(_.nonEmpty)
}