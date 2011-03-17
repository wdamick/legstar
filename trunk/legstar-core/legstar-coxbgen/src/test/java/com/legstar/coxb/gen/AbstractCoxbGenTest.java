/*******************************************************************************
 * Copyright (c) 2010 LegSem.
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

import org.apache.commons.io.FileUtils;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.util.BindingUtil;
import com.legstar.coxb.util.ClassUtil;
import com.legstar.jaxb.AbstractJaxbGenTest;

/**
 * This is code common to all junit tests that exercise the velocity templates.
 */
public abstract class AbstractCoxbGenTest extends AbstractJaxbGenTest {

    /** Location of JAXB classes. */
    public static final File JAXB_BIN_DIR = new File("target/classes");

    /** Ant scripts files will be generated here. */
    public static final File GEN_ANT_DIR = new File("target/src/gen/ant");

    /** Reference to binaries location. */
    public static final File GEN_BIN_DIR = new File("target/gen-classes");

    /** Sub directory for custom classes. */
    public static final String GEN_CUST_SUBDIR = "com/legstar/coxb/cust";

    /** New COBOL-annotated XML schema test cases. */
    public static final File COB_XSD_DIR = new File(
            "../legstar-jaxbgen/src/test/resources/cobxsd");

    /** Additional parameter set passed to templates. */
    private Map < String, Object > mParameters;

    /** @{inheritDoc */
    public void setUp() throws Exception {
        super.setUp();
        CodeGenUtil.initVelocity();
        mParameters = new HashMap < String, Object >();
        CodeGenHelper helper = new CodeGenHelper();
        mParameters.put("helper", helper);
        mParameters.put("coxbHelper", new CoxbHelper());
        FileUtils.forceMkdir(GEN_SRC_DIR);
        FileUtils.cleanDirectory(GEN_SRC_DIR);
        FileUtils.forceMkdir(GEN_ANT_DIR);
        FileUtils.cleanDirectory(GEN_ANT_DIR);
        FileUtils.forceMkdir(GEN_BIN_DIR);
        FileUtils.cleanDirectory(GEN_BIN_DIR);
    }

    /**
     * Reads a complete COXB source file into a string.
     * 
     * @param schemaName the schema used to generate
     * @param className the generated class name
     * @return a String with the class content
     */
    public String getCoxbSource(final String schemaName, final String className) {
        File srcFile = new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + '/' + schemaName
                + "/bind/" + className + ".java");
        return getSource(srcFile);
    }

    /**
     * The common target folder for generated binding files.
     * 
     * @param schemaName the schema name
     * @return a folder for binding files
     * @throws IOException if target folder cannot be created
     */
    public File getTargetFolder(final String schemaName) throws IOException {
        File targetFolder = new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + '/'
                + schemaName + "/bind/");
        FileUtils.forceMkdir(targetFolder);
        return targetFolder;
    }

    /**
     * @param schemaName the originating XSD name
     * @return the qualified custom class file name
     */
    public File getGetCustFilename(final String schemaName) {
        return new File(GEN_SRC_DIR, GEN_CUST_SUBDIR + '/' + schemaName
                + "/ChoiceSelector.java");
    }

    /**
     * @return the mParameters
     */
    public Map < String, Object > getParameters() {
        return mParameters;
    }

    /**
     * Check all generated binding schemas against a reference.
     * 
     * @param schemaName the schema name
     * @throws Exception if test fails
     */
    public void check(final String schemaName) throws Exception {
        check(new File(SRC_REF_DIR, GEN_SRC_SUBDIR + "/" + schemaName + "/bind"),
                new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + "/" + schemaName
                        + "/bind"), "java");
    }

    /**
     * Create a complex binding for a root element.
     * 
     * @param schemaName the schema name
     * @param rootName the root element name
     * @return a complex binding
     * @throws Exception if binding cannot be created
     */
    public ICobolComplexBinding getComplexBinding(final String schemaName,
            final String rootName) throws Exception {
        String packageName = JAXB_PKG_PFX + "." + schemaName;
        Object objectFactory = BindingUtil.newJaxbObjectFactory(packageName);
        return new CComplexReflectBinding(objectFactory,
                ClassUtil.loadClass(packageName + "." + rootName));
    }

    /**
     * Check a result against a reference.
     * 
     * @param schemaName the schema name
     * @param fileName the file name to check
     * @throws Exception if something fails
     */
    public void check(final String schemaName, final String fileName)
            throws Exception {
        File refFolder = new File(SRC_REF_DIR, GEN_SRC_SUBDIR + "/"
                + schemaName + "/bind");
        File resultFolder = new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + "/"
                + schemaName + "/bind");

        check(schemaName, fileName, refFolder, resultFolder);
    }

    /**
     * Create a common set of parameters.
     * 
     * @param schemaName the test case schema
     * @return a set of parameters with te right packages
     */
    public CoxbGenModel createModel(final String schemaName) {
        String packageName = JAXB_PKG_PFX + "." + schemaName;
        CoxbGenModel coxbContext = new CoxbGenModel();
        coxbContext.setJaxbPackageName(packageName);
        coxbContext.setCoxbPackageName(packageName + ".bind");
        coxbContext.setCoxbSrcDir(GEN_SRC_DIR);
        return coxbContext;
    }

}
