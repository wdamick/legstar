/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.jaxb;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Collection;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.w3c.dom.Document;

import com.legstar.codegen.CodeGenUtil;

/**
 * Methods that are useful for all test cases.
 * 
 */
public abstract class AbstractJaxbGenTest extends TestCase {

    /** Generated JAXB classes package prefix. */
    public static final String JAXB_PKG_PFX = "com.legstar.test.coxb";

    /** Target location for generated JAXB classes. */
    public static final File GEN_SRC_DIR = new File("target/src/gen/java");

    /** All java package names will have the same prefix. */
    public static final String GEN_SRC_SUBDIR = "com/legstar/test/coxb";

    /** Target location for generated XJB files. */
    public static final File GEN_XJB_DIR = new File("target/xjb");

    /** Maven should have populated this location with test schemas. */
    public static final File XSD_DIR = new File("../target/cases/schema");

    /** New COBOL-annotated XML schema test cases. */
    public static final File COB_XSD_DIR = new File("src/test/resources/cobxsd");

    /** Generated classes Reference folder. */
    public static final File SRC_REF_DIR = new File("src/test/java");

    /** Reference files which are not sources. */
    public static final File REF_DIR = new File("src/test/resources/reference");

    /** This means references should be created instead of compared to results. */
    private boolean _createReferences = false;

    /** Helper to create DOM documents. */
    private DocumentBuilder _db;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Make sure we have an output folder.
     * 
     * @throws Exception if output folder cannot be created
     */
    public void setUp() throws Exception {
        CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
        FileUtils.cleanDirectory(GEN_SRC_DIR);
        CodeGenUtil.checkDirectory(GEN_XJB_DIR, true);
        FileUtils.cleanDirectory(GEN_XJB_DIR);
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _db = docFac.newDocumentBuilder();
    }

    /**
     * Reads a complete JAXB source file into a string.
     * 
     * @param schemaName the schema used to generate
     * @param className the generated class name
     * @return a String with the class content
     */
    public String getJaxbSource(final String schemaName, final String className) {
        return getSource(schemaName, GEN_SRC_SUBDIR, className);
    }

    /**
     * Reads a complete source file into a string.
     * 
     * @param schemaName the schema used to generate
     * @param srcSubDir the source sub directory (based on package name)
     * @param className the generated class name
     * @return a String with the class content
     */
    public String getSource(final String schemaName, final String srcSubDir,
            final String className) {
        File srcFile = new File(GEN_SRC_DIR, srcSubDir + '/' + schemaName + '/'
                + className + ".java");
        return getSource(srcFile);
    }

    /**
     * Reads a complete source file into a string.
     * 
     * @param schemaName the schema used to generate
     * @param srcSubDir the source sub directory (based on package name)
     * @param className the generated class name
     * @return a String with the class content
     */
    public String getSource(final String srcFileName) {
        File srcFile = new File(srcFileName);
        return getSource(srcFile);
    }

    /**
     * Reads a complete source file into a string.
     * 
     * @param srcFile the source file to read into a string
     * @return a String with the class content
     */
    public String getSource(final File srcFile) {
        try {
            String result = FileUtils.readFileToString(srcFile);
            if (_log.isDebugEnabled()) {
                _log.debug(srcFile);
                _log.debug(result);
            }
            return result;
        } catch (IOException e) {
            fail("Source file " + srcFile.toString() + " was not generated");
            return null;
        }
    }

    /**
     * Loads an XML Schema file into an in-memory model.
     * 
     * @param source the XML Schema content
     * @return a in-memory XML SChema model
     */
    public XmlSchema getXmlSchema(final String source) {
        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        return schemaCol.read(new StringReader(source), null);
    }

    /**
     * @return true if references should be created instead of compared to
     *         results
     */
    public boolean isCreateReferences() {
        return _createReferences;
    }

    /**
     * @param createReferences true if references should be created instead of
     *            compared to results
     */
    public void setCreateReferences(boolean createReferences) {
        _createReferences = createReferences;
    }

    /**
     * Return a class unqualified (no package prefix) name.
     * 
     * @param clazz the class
     * @return the unqualified name
     */
    public static String getUnqualName(final Class < ? > clazz) {
        String unqname = clazz.getName();
        if (unqname.lastIndexOf('.') > 0) {
            unqname = unqname.substring(unqname.lastIndexOf('.') + 1);
        }
        return unqname;
    }

    /**
     * Check a result against a reference.
     * <p/>
     * Here result is a folder as well a reference. In check mode all file are
     * compared in creation mode all reference files are created as copies from
     * the results.
     * <p/>
     * ObjectFactories are not compared because their content is not ordered in
     * a consistent way.
     * 
     * @param refFolder the reference folder (containing reference files)
     * @param resultFolder the result folder (containing generated files)
     * @param extension the files extension to process
     * @throws Exception if something fails
     */
    @SuppressWarnings("unchecked")
    public void check(final File refFolder, final File resultFolder,
            final String extension) throws Exception {

        if (isCreateReferences()) {
            Collection < File > resultFiles = FileUtils.listFiles(resultFolder,
                    new String[] { extension }, false);
            for (File resultFile : resultFiles) {
                FileUtils.copyFileToDirectory(resultFile, refFolder);
            }
        } else {
            Collection < File > referenceFiles = FileUtils.listFiles(refFolder,
                    new String[] { extension }, false);
            for (File referenceFile : referenceFiles) {
                if (referenceFile.getName().equals("ObjectFactory.java")) {
                    continue;
                }
                File resultFile = new File(resultFolder,
                        FilenameUtils.getName(referenceFile.getPath()));
                assertEquals(referenceFile, resultFile);
            }
        }

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
                + schemaName);
        File resultFolder = new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + "/"
                + schemaName);

        check(schemaName, fileName, refFolder, resultFolder);
    }

    /**
     * Check a result against a reference stored at the unit test level (not to
     * be made available for integration testing).
     * 
     * @param schemaName the schema name
     * @param fileName the file name to check
     * @throws Exception if something goes wrong
     */
    public void checkLocalRef(final String schemaName, final String fileName)
            throws Exception {
        File resultFolder = new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + "/"
                + schemaName);
        File refFolder = new File(REF_DIR, getClass().getSimpleName());
        check(schemaName, fileName, refFolder, resultFolder);
    }

    /**
     * Check a result against a reference.
     * 
     * @param schemaName the schema name
     * @param fileName the file name to check
     * @param refFolder the reference folder
     * @param resultFolder the result folder
     * @throws Exception if something fails
     */
    public void check(final String schemaName, final String fileName,
            final File refFolder, final File resultFolder) throws Exception {
        File resultFile = new File(resultFolder, fileName);

        if (isCreateReferences()) {
            FileUtils.copyFileToDirectory(resultFile, refFolder);
        } else {
            File referenceFile = new File(refFolder, fileName);
            assertEquals(referenceFile, resultFile);
        }
    }

    /**
     * When comparing file contents we neutralize any platform specific line
     * ending character such as CR (\r).
     * 
     * @param referenceFile the expected file
     * @param resultFile the result file
     * @throws Exception if something fails
     */
    protected void assertEquals(final File referenceFile, final File resultFile)
            throws Exception {
        String expected = FileUtils.readFileToString(referenceFile);
        String result = FileUtils.readFileToString(resultFile);
        assertEquals(
                String.format("comparing result file %s with %s",
                        resultFile.getName(), referenceFile.getName()),
                expected.replace("\r", ""), result.replace("\r", ""));
    }

    /**
     * Returns the XML schema contant in a string.
     * 
     * @param xsd the XML Schema
     * @return a string holding the schema content
     */
    public String toString(final XmlSchema xsd) {
        StringWriter writer = new StringWriter();
        xsd.write(writer);
        if (_log.isDebugEnabled()) {
            _log.debug(writer.toString());
        }
        return writer.toString();
    }

    /**
     * @return a new DOM document
     */
    public Document newDocument() {
        return _db.newDocument();
    }

}
