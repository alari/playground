package learn.text;

import org.pegdown.Printer;
import org.pegdown.ast.Node;
import org.pegdown.ast.Visitor;
import org.pegdown.plugins.ToHtmlSerializerPlugin;

public class SectionSerializer implements ToHtmlSerializerPlugin {
    @Override
    public boolean visit(Node node, Visitor visitor, Printer printer) {
        System.out.println(node);
        if (node instanceof TagNode) {
            TagNode cNode = (TagNode)node;
            cNode.print(printer);
            return true;
        }
        return false;
    }
}
