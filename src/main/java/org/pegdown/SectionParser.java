package org.pegdown;

import learn.text.SectionProcessor;
import learn.text.TagNode;
import org.parboiled.BaseParser;
import org.parboiled.Rule;
import org.parboiled.support.StringBuilderVar;
import org.pegdown.plugins.BlockPluginParser;

public class SectionParser extends Parser implements BlockPluginParser {
    final String TagMarker = "%%";

    final String tag;

    public SectionParser(int options, String tag){
        super(options, 2000l, DefaultParseRunnerProvider);
        this.tag = tag;
    }

    public SectionParser(String tag) {
        this(SectionProcessor.OPTIONS, tag);
    }

    public Rule[] blockPluginRules() {
        return new Rule[]{ component() };
    }

    Rule component() {
        return NodeSequence(open(), body(), close(), push(new TagNode(tag, popAsNode())));
    }

    Rule body() {
        StringBuilderVar rawBody = new StringBuilderVar();

        return Sequence(
                OneOrMore(
                        TestNot(TagMarker),
                        BaseParser.ANY,
                        rawBody.append(matchedChar())),
                push(parseInternal(rawBody)));
    }

    Rule open() {
        return Sequence(
                TagMarker,
                tag,
                Newline()
        );
    }

    String close() {
        return TagMarker;
    }

}
