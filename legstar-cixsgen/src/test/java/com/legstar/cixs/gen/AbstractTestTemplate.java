package com.legstar.cixs.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.gen.CoxbHelper;

import junit.framework.TestCase;

/**
 * This is code common to all junit tests that exercise the velocity
 * templates.
 */
public class AbstractTestTemplate extends TestCase {

    /** Code will be generated here. */
    public static final File GEN_SRC_DIR = new File("src/test/gen/java");

    /** General location for generated artifacts. */
    public static final File GEN_RES_DIR = new File(
            "src/test/gen/resources");

    /** Property files will be generated here. */
    public static final File GEN_PROP_DIR = GEN_RES_DIR;

    /** Ant scripts will be generated here. */
    public static final File GEN_ANT_DIR = new File("ant");
    
    /** Configuration files will be generated here. */
    public static final File GEN_CONF_DIR = new File("conf");
    
    /** Additional parameter set passed to templates */
    private Map <String, Object> mParameters;
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(
            AbstractTestTemplate.class);
    
    /** @{inheritDoc}*/
    @Override
    public void setUp() {
        try {
            CodeGenUtil.initVelocity();
           	CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
            mParameters = new HashMap <String, Object>();
            CodeGenHelper helper = new CodeGenHelper();
            mParameters.put("helper", helper);
            mParameters.put("coxbHelper", new CoxbHelper());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    
    /**
     * A general purpose reader that gets the file content into a string.
     * @param srcDir the location of the source artifact
     * @param srcName the source artifact name
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(
            File srcDir, String srcName) throws IOException {
    	return getSource(new File(srcDir, srcName));
    }
    
    /**
     * A general purpose reader that gets the file content into a string.
     * @param fileName the name of the file pointing to source
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(String fileName) throws IOException {
    	return getSource(new File(fileName));
    }
    /**
     * A general purpose reader that gets the file content into a string.
     * @param file the file pointing to source
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(File file) throws IOException {
        BufferedReader in = new BufferedReader(
                new FileReader(file));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            LOG.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        return resStr;
    }


    /**
     * @return the mParameters
     */
    public final Map<String, Object> getParameters() {
        return mParameters;
    }


}
