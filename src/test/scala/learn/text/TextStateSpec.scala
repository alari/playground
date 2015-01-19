package learn.text

import org.specs2.mutable.Specification

class TextStateSpec extends Specification {
  "text state" should {
    "be initiated" in {
      val cmd = CommitText(Committer("1"), "test\nme")
      println(cmd.toInit)
      val s0 = TextState.initEvent(cmd.toInit)
      println(s0)

      s0.text must_== cmd.text
    }
  }
}
