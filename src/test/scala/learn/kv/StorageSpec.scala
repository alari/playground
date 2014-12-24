package learn.kv

import org.specs2.mutable.Specification

class StorageSpec extends Specification{
  implicit object append extends ValueAppender[Any] {
    override def wrap(value: Any): Any = Vector(value)

    override def append(to: Any, values: Any*): Any = to match {
      case t: Vector[_] => t ++ values
      case _ => Vector(to) ++ values
    }
  }

  append.isCorrect(1, 2)

  val empty = KeyValueStorage[Any]()

  "key-value storage" should {
    "save key" in {
      empty.get("test") should beNone

      empty.keys() should beEmpty

      val wk = empty.set("test", "any", 0)

      wk.get("test").map(_.value) should beSome("any")

      wk.list().exists(kv => kv._1 == "/test" && kv._2.value == "any") should beTrue
      wk.keys().contains("/test") should beTrue
      wk.list().size should_== 1
      wk.keys().size should_== 1

      val wk2 = wk.process(Command.Set("/test/subkey/2", "other", 0))

      wk2.value("test") should beSome("any")

      wk2.get("/test/subkey") should beNone

      wk2.get("test/subkey/2").map(_.value) should beSome("other")

      wk2.list().size should_== 2

      wk2.list("/test").exists(kv => kv._1 == "/test" && kv._2.value == "any") should beTrue

      wk2.list().exists(kv => kv._1 == "/test/subkey/2" && kv._2.value == "other") should beTrue

      wk2.list("test/subkey").size should_== 1

      wk2.list("test/subkey").exists(kv => kv._1 == "/test/subkey/2") should beTrue

      wk2.keys().contains("/test") should beTrue
      wk2.keys().contains("/test/subkey/2") should beTrue
      wk2.keys().size should_== 2
    }

    "correctly expire and drop keys" in {
      val wk = empty.set("test", "any", 3).set("test/subkey", "other", 2).set("/test/subkey/3", "value", 1)

      wk.list().size should_== 3
      wk.list("test").size should_== 3
      wk.list("some").size should_== 0

      wk.expire(5).isEmpty should beTrue

      println(wk.expireList(5))

      wk.expireList(5)._2.isEmpty should beTrue

      wk.expire(3).isEmpty should beFalse
      wk.expire(3) should_== wk.expireList(3)._2

      wk.expire(3).list().size should_== 1
      wk.expireList(3)._1.size should_== 2

      wk.expire(2).list().size should_== 2

      val wk2 = wk.set("test/sub", "check", 5)

      wk.expire(4).isEmpty should beTrue
    }

    "remove keys" in {
      val wk = empty.set("test", "any", 3).set("test/subkey", "other", 2).set("test/subkey/3", "value", 1)

      wk.list().size should_== 3

      wk.delete("/test/subkey").list().size should_== 1
    }

    "add values" in {
      val wk = empty.set("test/some", "any", 3)

      wk.list().size should_== 1

      wk.add("test/some", "other", 3).list().size should_== 1

      wk.add("/test/some", "other", 3).list().exists(_._2.value match {
        case v: Vector[_] => v.contains("any") && v.contains("other")
        case _ => false
      }) should beTrue
    }

    "handle empty key" in {
      val wk = empty.set("", "root", 3)

      wk should_== empty.copy(value = Some(Value[Any]("root", 3)))

      println("wk -> "+wk)
      println("wklist -> "+wk.list)

      wk.list().size should_== 1

      wk.value("") should beSome("root")
      wk.value("/") should beSome("root")

      val wk2 = wk.set("/", "other", 5)
      wk2.value("") should beSome("other")
    }
  }
}
