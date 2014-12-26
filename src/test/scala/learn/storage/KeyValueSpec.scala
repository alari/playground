package learn.storage

import org.specs2.mutable.Specification

class KeyValueSpec extends Specification {

  val empty = KeyValue(Map.empty[String,KeyValue[String]])

  "key-value storage" should {
    "save key" in {
      empty("test") should beNone

      empty.flatten should beEmpty

      val wk = empty.update("test", "any")

      wk("test") should beSome("any")

      println(wk.flatten)

      wk.flatten should_== Map("/test" -> "any")

      wk.flatten.exists(kv => kv._1 == "/test" && kv._2 == "any") should beTrue
      wk.flatten.size should_== 1

      val wk2 = wk.update("/test/subkey/2", "other")

      wk2("test") should beSome("any")

      wk2("/test/subkey") should beNone

      wk2("test/subkey/2") should beSome("other")

      wk2.flatten.size should_== 2

      wk2.flattenBranch("/test").exists(kv => kv._1 == "/test" && kv._2 == "any") should beTrue

      wk2.flatten.exists(kv => kv._1 == "/test/subkey/2" && kv._2 == "other") should beTrue

      wk2.flattenBranch("test/subkey").size should_== 1

      println(wk2.flattenBranch("test/subkey"))

      wk2.flattenBranch("test/subkey").exists(kv => kv._1 == "/test/subkey/2") should beTrue
    }

    "correctly span and drop keys" in {
      val wk = empty.update("test", "any").update("test/subkey", "ot").update("/test/subkey/3", "v")

      wk.flatten.size should_== 3
      wk.flattenBranch("test").size should_== 3
      wk.flattenBranch("some").size should_== 0


      println(wk.span(_.size > 5))

      wk.span(_.size > 5)._1.isEmpty should beTrue

//
//      wk.span(_.size > 5)._2.isEmpty should beFalse
//
//      wk.span(_.size > 3)._1.isEmpty should beFalse
//      wk.expire(3) should_== wk.expireList(3)._2
//
//      wk.expire(3).list().size should_== 1
//      wk.expireList(3)._1.size should_== 2
//
//      wk.expire(2).list().size should_== 2
//
//      val wk2 = wk.update("test/sub", "check", 5)
//
//      wk.expire(4).isEmpty should beTrue
    }

    "remove keys" in {
      val wk = empty.update("test", "any").update("test/subkey", "other").update("test/subkey/3", "value")

      wk.flatten.size should_== 3

      wk.removeBranch("/test/subkey").flatten.size should_== 1
    }
//
//    "add values" in {
//      val wk = empty.set("test/some", "any", 3)
//
//      wk.list().size should_== 1
//
//      wk.add("test/some", "other", 3).list().size should_== 1
//
//      wk.add("/test/some", "other", 3).list().exists(_._2.value match {
//        case v: Vector[_] => v.contains("any") && v.contains("other")
//        case _ => false
//      }) should beTrue
//    }

    "handle empty key" in {
      val wk = empty.update("", "root")

      wk.value should beSome("root")

      wk.flatten.size should_== 1

      wk("") should beSome("root")
      wk("/") should beSome("root")

      val wk2 = wk.update("/", "other")
      wk2("") should beSome("other")
    }
  }
}
