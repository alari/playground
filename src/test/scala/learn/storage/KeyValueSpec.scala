package learn.storage

import org.specs2.mutable.Specification

import scalaz.Monoid

class KeyValueSpec extends Specification {

  val empty = KeyValue(Map.empty[String,KeyValue[String]])

  "key-value storage" should {
    "save key" in {
      empty("test") should beNone

      empty.flatten should beEmpty

      val wk = empty.update("test", "any")

      wk("test") should beSome("any")

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

      wk2.flattenBranch("test/subkey").exists(kv => kv._1 == "/test/subkey/2") should beTrue
    }

    "correctly span and drop keys" in {
      val wk = empty.update("test", "any").update("test/subkey", "ot").update("/test/subkey/3", "v")

      wk.flatten.size should_== 3
      wk.flattenBranch("test").size should_== 3
      wk.flattenBranch("some").size should_== 0

      wk.span(_.size > 5)._1.isEmpty should beTrue
      wk.span(_.size > 5)._2 should_== wk

      wk.span(_.size > 2)._1 should_== empty.update("test", "any")
      wk.span(_.size > 2)._2 should_== empty.update("test/subkey", "ot").update("/test/subkey/3", "v")

      wk.span(_.size > 1)._1 should_== empty.update("test", "any").update("test/subkey", "ot")
      wk.span(_.size > 1)._2 should_== empty.update("/test/subkey/3", "v")

    }

    "remove keys" in {
      val wk = empty.update("test", "any").update("test/subkey", "other").update("test/subkey/3", "value")

      wk.flatten.size should_== 3

      wk.removeBranch("/test/subkey").flatten.size should_== 1
    }

    "add values" in {
      implicit val m = Monoid.instance[String](_ + _, "")

      val wk = empty.update("test/some", "any")

      wk.flatten.size should_== 1

      wk.add("test/some", "other").flatten.size should_== 1

      wk.add("/test/some", "other").flatten.apply("/test/some") should_== "anyother"
    }

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
