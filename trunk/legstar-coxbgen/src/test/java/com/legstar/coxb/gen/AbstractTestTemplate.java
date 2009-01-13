/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

/**
 * This is code common to all junit tests that exercise the velocity
 * templates.
 */
public class AbstractTestTemplate extends TestCase {

    /** Parent generation folder. */
    public static final File GEN_DIR = new File("target/src/gen");
    
    /** Location of JAXB classes. */
    public static final File JAXB_BIN_DIR = new File("target/classes");

    /** Code will be generated here. */
    public static final File GEN_SRC_DIR = new File("target/src/gen/java");

    /** Configuration files will be generated here. */
    public static final File GEN_CONF_DIR = new File("target/src/gen/conf");
    
    /** Ant scripts files will be generated here. */
    public static final File GEN_ANT_DIR = new File("target/src/gen/ant");
    
    /** Properties files will be generated here. */
    public static final File GEN_PROP_DIR = new File("target/src/gen/prop");
    
    /** Reference to binaries location. */
    public static final File GEN_BIN_DIR = new File("target/gen-classes");

    /** COBOL code will be generated here. */
    public static final File GEN_COBOL_DIR = new File("target/src/gen/cobol");
    
    /** Additional parameter set passed to templates. */
    private Map < String, Object > mParameters;
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(AbstractTestTemplate.class);
    
    /** @{inheritDoc}*/
    public void setUp() {
        try {
            CodeGenUtil.initVelocity();
            mParameters = new HashMap < String, Object >();
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
     * @throws Exception if something goes wrong
     */
    public String getSource(
            final File srcDir, final String srcName) throws Exception {
        BufferedReader in = new BufferedReader(
                new FileReader(new File(srcDir, srcName)));
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
    public final Map < String, Object > getParameters() {
        return mParameters;
    }

    /**
     * Recreates a folder after emptying its content.
     * @param dir the folder to empy
     */
    public void emptyDir(final File dir) {
        deleteDir(dir);
        dir.mkdirs();
    }
    
    /**
     * Destroys a folder and all of its content.
     * @param dir the folder to destroy
     */
    public void deleteDir(final File dir) {
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

