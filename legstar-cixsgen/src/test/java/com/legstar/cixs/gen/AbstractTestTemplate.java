package com.legstar.cixs.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.gen.CoxbHelper;

import junit.framework.TestCase;

/**
 * This is code common to all junit tests that exercise the velocity
 * templates.
 */
public class AbstractTestTemplate extends TestCase {

    /** Parent generation folder. */
    public static final File GEN_DIR = new File("src/test/gen");
    
    /** Location of JAXB classes. */
    public static final File JAXB_BIN_DIR = new File("target/classes");

    /** Code will be generated here. */
    public static final File GEN_SRC_DIR = new File("src/test/gen/java");

    /** Configuration files will be generated here. */
    public static final File GEN_CONF_DIR = new File("src/test/gen/conf");
    
    /** Cobol files will be generated here. */
    public static final File GEN_COBOL_DIR = new File("src/test/gen/cobol");
    
    /** Web descriptors files will be generated here. */
    public static final File GEN_WDD_DIR = new File("src/test/gen/webapp");
    
    /** Web descriptors files will be generated here. */
    public static final File GEN_ANT_DIR = new File("src/test/gen/ant");
    
    /** Properties files will be generated here. */
    public static final File GEN_PROP_DIR = new File("src/test/gen/prop");
    
    /** Reference to binaries location. */
    public static final File GEN_BIN_DIR = new File("src/test/gen/target/classes");

    /** Reference to war files location. */
    public static final File GEN_WAR_DIR = new File("${env.CATALINA_BASE}/webapp");

    /** Additional parameter set passed to templates */
    private Map <String, Object> mParameters;
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(
            AbstractTestTemplate.class);
    
    /** @{inheritDoc}*/
    @Override
    public void setUp() {
        try {
        	emptyDir(GEN_DIR);
            CodeGenUtil.initVelocity();
           	CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
           	CodeGenUtil.checkDirectory(GEN_CONF_DIR, true);
           	CodeGenUtil.checkDirectory(GEN_COBOL_DIR, true);
           	CodeGenUtil.checkDirectory(GEN_WDD_DIR, true);
           	CodeGenUtil.checkDirectory(GEN_ANT_DIR, true);
            mParameters = new HashMap <String, Object>();
            CodeGenHelper helper = new CodeGenHelper();
            mParameters.put("helper", helper);
            mParameters.put("coxbHelper", new CoxbHelper());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    
	/**
	 * Apply a velocity template and return the generated source.
	 * @param model model to use
	 * @param generatorName the name to appear as generator 
	 * @param templateName the velocity template to apply
	 * @param dir the folder where generated source should go
	 * @param genSourceName the generate file name
	 * @return the source code generated
	 * @throws Exception if something goes wrong
	 */
	public String genSource(
			CixsJaxwsService model,
			String generatorName,
			String templateName,
			File dir,
			String genSourceName) throws Exception {
		CodeGenUtil.processTemplate(
				generatorName,
				templateName,
				"model", model,
				getParameters(),
				CodeGenUtil.getFile(dir, genSourceName));
		return getSource(dir, genSourceName);
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

    /**
     * Recreates a folder after emptying its content.
     * @param dir the folder to empy
     */
    public void emptyDir(File dir) {
        deleteDir(dir);
        dir.mkdirs();
    }
    
    /**
     * Destroys a folder and all of its content.
     * @param dir the folder to destroy
     */
    public void deleteDir(File dir) {
        if (dir.exists()) {
            for (File file : dir.listFiles()) {
                if (file.isDirectory()) {
                    deleteDir(file);
                }
                file.delete();
            }
        }
    }

}
