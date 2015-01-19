package learn.text;

import org.pegdown.Extensions;
import org.pegdown.SectionParser;
import org.pegdown.plugins.PegDownPlugins;

public class SectionProcessor {
    final static public int OPTIONS = Extensions.HARDWRAPS + Extensions.AUTOLINKS + Extensions.SMARTYPANTS;

    static public PegDownPlugins plugins() {
        return new PegDownPlugins.Builder().withPlugin(SectionParser.class, "poetry").withPlugin(SectionParser.class, "стихи").build();
    }


}
