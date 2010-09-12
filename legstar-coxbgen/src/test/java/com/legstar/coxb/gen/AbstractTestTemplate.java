/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.gen;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;

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

    /** Maven should have populated this location with test schemas. */
    public static final File XSD_DIR = new File("../target/cases/schema");

    /** Additional parameter set passed to templates. */
    private Map < String, Object > mParameters;

    /** @{inheritDoc */
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
     * 
     * @param srcDir the location of the source artifact
     * @param srcName the source artifact name
     * @return a string containing the generated source
     */
    public String getSource(
            final File srcDir, final String srcName) {
        try {
            return FileUtils.readFileToString(new File(srcDir, srcName));
        } catch (IOException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
        return null;
    }

    /**
     * @return the mParameters
     */
    public Map < String, Object > getParameters() {
        return mParameters;
    }

    /**
     * Lookup a test schema from a folder.
     * 
     * @param schemaName the schema name
     * @return the schema file
     * @throws Exception if schema file cannot be located
     */
    protected File getSchemaFromFolder(final String schemaName)
            throws Exception {
        String fileName = schemaName;
        if (Character.isLowerCase(schemaName.charAt(0))) {
            fileName = schemaName.toUpperCase() + ".xsd";
        } else {
            fileName = schemaName + ".xsd";
        }

        return new File(XSD_DIR.getCanonicalPath() + '/' + fileName);
    }

}
