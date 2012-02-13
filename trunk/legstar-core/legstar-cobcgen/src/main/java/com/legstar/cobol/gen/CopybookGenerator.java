package com.legstar.cobol.gen;

import java.util.Arrays;

import org.antlr.stringtemplate.AttributeRenderer;
import org.antlr.stringtemplate.CommonGroupLoader;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateErrorListener;
import org.antlr.stringtemplate.StringTemplateGroup;
import org.antlr.stringtemplate.language.DefaultTemplateLexer;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cobol.model.CobolDataItem;

/**
 * Generates a COBOL copybook from a model using templates.
 * 
 */
public class CopybookGenerator {

    public static final String TEMPLATE_BASE = "templates";

    public static final String COPYBOOK_TEMPLATE_GROUP_NAME = "cobol-group";

    public static final String COPYBOOK_TEMPLATE_NAME = "toCobolCopybook";

    /** Size of the sequence number area. */
    private static final int SEQUENCE_NUMBER_AREA_SIZE = 6;

    /** Size of the indicator area (comments and continuation markers). */
    private static final int INDICATOR_AREA_SIZE = 1;

    /** Size of the area A (between column 8 and 2). */
    private static final int AREA_A_SIZE = 4;

    /** How much indent characters from parent in area B. */
    private static final int AREA_B_INDENT_INCR = 2;

    private static Log logger = LogFactory.getLog(CopybookGenerator.class);

    /** Utility class. */
    private CopybookGenerator() {

    }

    /**
     * Fetches templates from the classpath.
     */
    public static final CommonGroupLoader GROUP_LOADER = new CommonGroupLoader(
            TEMPLATE_BASE, new StringTemplateErrorListener() {
                public void error(String msg, Throwable e) {
                    logger.error(msg, e);
                }

                public void warning(String msg) {
                    logger.warn(msg);
                }
            });

    /**
     * Generates a COBOL copybook as a string.
     * 
     * @param cobolDataItem the COBOL data item
     * @return the COBOL copybook content
     */
    public static String generate(CobolDataItem cobolDataItem) {
        StringTemplate copybookTemplate = getTemplate(
                COPYBOOK_TEMPLATE_GROUP_NAME, COPYBOOK_TEMPLATE_NAME);
        copybookTemplate.registerRenderer(Integer.class,
                new StaticIntegerRenderer());
        copybookTemplate.setAttribute("cobolDataItem", cobolDataItem);
        return copybookTemplate.toString();
    }

    /**
     * Retrieve a template from the classpath.
     * 
     * @param templateGroupName the template group name
     * @param templateName the template name
     * @return the template instance
     */
    public static StringTemplate getTemplate(final String templateGroupName,
            final String templateName) {
        StringTemplateGroup group = GROUP_LOADER.loadGroup(templateGroupName,
                DefaultTemplateLexer.class, null);
        group.setStringTemplateWriter(Copybook72ColWriter.class);
        return group.getInstanceOf(templateName);
    }

    /**
     * Certain numeric types have to be formatted in special ways.
     * 
     */
    private static class StaticIntegerRenderer implements AttributeRenderer {

        /** Last COBOL level processed. */
        private int lastLevel = -1;

        /** Last depth encountered in the COBOL structure hierarchy. */
        private int lastDepth = -1;

        public String toString(Object attribute) {
            return ((Integer) attribute).toString();
        }

        /**
         * Formats the COBOL numeric attributes such as level and depth.
         * <p/>
         * Depth is important to indent COBOL properly. This assumes items are
         * visited in the natural ordering in the COBOL structure tree
         * (Breadth-first traversal).
         * 
         * */
        public String toString(Object attribute, String formatName) {
            if (formatName.equals("level")) {
                return String.format("%02d", (Integer) attribute);
            } else if (formatName.equals("depth")) {
                int level = (Integer) attribute;
                int depth = 0;
                if (level < lastLevel) {
                    depth = lastDepth -= 1;
                } else if (level == lastLevel) {
                    depth = lastDepth;
                } else {
                    depth = lastDepth += 1;
                }
                char[] chars = new char[SEQUENCE_NUMBER_AREA_SIZE
                        + INDICATOR_AREA_SIZE
                        + ((depth > 0) ? AREA_A_SIZE + (depth - 1)
                                * AREA_B_INDENT_INCR : 0)];
                Arrays.fill(chars, ' ');
                lastLevel = level;
                lastDepth = depth;
                return new String(chars);
            }
            return String.format(formatName, (Integer) attribute);
        }

    }

}
