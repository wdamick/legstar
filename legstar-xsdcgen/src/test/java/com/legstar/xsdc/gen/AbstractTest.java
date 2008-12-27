package com.legstar.xsdc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import junit.framework.TestCase;

/**
 * Common test code.
 */
public abstract class AbstractTest extends TestCase {

    /** Location of test schemas. */
    public static final File SCHEMA_DIR = new File("src/test/resources");

    /** Target location for generated schemas. */
    public static final File GEN_DIR = new File("target/src/gen");
    
    /** An instance of the Xsd to COBOL annaotator. */
    private XsdCobolAnnotator mXsdCobolAnnotator;
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(AbstractTest.class);
    /**
     * @return the Xsd to Cobol Annotator
     */
    public XsdCobolAnnotator getXsdCobolAnnotator() {
        return mXsdCobolAnnotator;
    }

    /** {@inheritDoc} */
    public void setUp() {
        mXsdCobolAnnotator = new XsdCobolAnnotator();
        GEN_DIR.mkdirs();
        mXsdCobolAnnotator.setTargetDir(GEN_DIR);
    }

    /**
     * Return the content of a file as a string.
     * @param srcDir the location of the source artifact
     * @param srcName the source artifact name
     * @return a String with file content
     * @throws IOException if reading file contents fails
     */
    public String getSource(final File srcDir, final String srcName) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(new File(srcDir, srcName)));
        StringBuffer res = new StringBuffer();
        String str = in.readLine();
        while (str != null) {
            LOG.debug(str);
            res.append(str);
            str = in.readLine();
        }
        in.close();
        return res.toString();
    }
    
    /**
     * Get a schema file as a URI.
     * @param fileName the name of the schema 
     * @return the URI
     */
    public URI getSchemaFileURI(final String fileName) {
        return new File(SCHEMA_DIR, fileName).toURI();
    }

}
