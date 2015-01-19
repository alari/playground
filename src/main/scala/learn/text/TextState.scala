package learn.text

import akka.http.util.DateTime
import difflib.DiffUtils
import scala.collection.JavaConversions._

case class Committer(id: String) extends AnyVal

case class TextState(committers: Set[Committer], text: String, created: DateTime, updated: DateTime)

object TextState {
  def applyPatch(text: String, diff: String): String = {
    DiffUtils.parseUnifiedDiff(diff.split("\n").toSeq).applyTo(text.split("\n").toSeq).mkString("\n")
  }

  def generateDiff(text: String, revised: String): String = {
    val textLines = text.split("\n").toSeq
    val revisedLines = revised.split("\n").toSeq
    DiffUtils.generateUnifiedDiff("original", "target", textLines, DiffUtils.diff(textLines, revisedLines), 0).mkString("\n")
  }

  def applyEvent(state: TextState, event: CommitEvent) = state.copy(
    committers = state.committers + event.committer,
    text = applyPatch(state.text, event.diff),
    updated = event.timestamp
  )

  def initEvent(event: CommitEvent): TextState = TextState(committers = Set(event.committer), text = applyPatch("", event.diff), created = event.timestamp, updated = event.timestamp)
}

case class CommitDiff(committer: Committer, diff: String) {
  def toEvent(state: TextState) = toInit
  def toInit = CommitEvent(committer = committer, diff = diff)
}

case class CommitText(committer: Committer, text: String) {
  def toEvent(state: TextState) = CommitEvent(committer = committer, diff = TextState.generateDiff(state.text, text))
  def toInit = CommitEvent(committer = committer, diff = TextState.generateDiff("", text))
}

case class CommitEvent(committer: Committer, diff: String, timestamp: DateTime = DateTime.now)