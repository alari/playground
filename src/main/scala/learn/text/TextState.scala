package learn.text

import java.util

import akka.http.util.DateTime
import difflib.DiffUtils

case class Committer(id: String) extends AnyVal

case class TextState(committers: Set[Committer], text: String, created: DateTime, updated: DateTime)

object TextState {
  def applyPatch(text: String, patch: String): String = {
//    DiffUtils.patch()
//
//    val dmp = new DiffMatchPatch()
//    dmp.patch_apply(dmp.patch_fromText(patch).asInstanceOf[util.LinkedList[DiffMatchPatch.Patch]], text).head.asInstanceOf[String]
    text + patch
  }

  def applyEvent(state: TextState, event: CommitEvent) = state.copy(
    committers = state.committers + event.committer,
    text = applyPatch(state.text, event.patch),
    updated = event.timestamp
  )

  def initEvent(event: CommitEvent): TextState = TextState(committers = Set(event.committer), text = applyPatch("", event.patch), created = event.timestamp, updated = event.timestamp)

}

case class Commit(committer: Committer, patch: String)

case class CommitEvent(committer: Committer, patch: String, timestamp: DateTime)