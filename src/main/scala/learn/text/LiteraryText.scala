package learn.text

import java.util

import org.pegdown.ast.{Visitor, AbstractNode, Node, SuperNode}
import org.pegdown.plugins.{ToHtmlSerializerPlugin, PegDownPlugins}
import org.pegdown._

case class LiteraryText(text: String) extends AnyVal {
  def serializePlugins: util.List[ToHtmlSerializerPlugin] =
    util.Arrays.asList(new SectionSerializer)

  def serializer = new ToHtmlSerializer(new LinkRenderer, serializePlugins)

  def toTree = new PegDownProcessor(Extensions.HARDWRAPS + Extensions.AUTOLINKS + Extensions.SMARTYPANTS, SectionProcessor.plugins()).parseMarkdown(text.toCharArray)

  def toHtml: String = serializer.toHtml(toTree)
}


class TagNode(tag: String, child: Node) extends SuperNode(child) {
  def print(printer: Printer) = {
    printer.print("<section class=\""+tag+"\">")
    //printer.print(content)
    printer.print("</section>")
  }

  override def accept(visitor: Visitor): Unit = visitor.visit(this)

  override def getChildren: util.List[Node] = new util.ArrayList
}