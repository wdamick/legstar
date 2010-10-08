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
package com.legstar.jaxb;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
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
public abstract class AbstractJaxbTester extends TestCase {

    /** Target location for generated JAXB classes. */
    public static final File GEN_SRC_DIR = new File("target/src/gen/java");

    /** All java package names will have the same prefix. */
    public static final String GEN_SRC_SUBDIR = "com/legstar/test/coxb";

    /** Target location for generated XJB files. */
    public static final File GEN_XJB_DIR = new File("target/xjb");

    /** Maven should have populated this location with test schemas. */
    public static final File XSD_DIR = new File("../target/cases/schema");

    /** Helper to create DOM documents. */
    private DocumentBuilder _db;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Make sure we have an output folder.
     * 
     * @throws Exception if output folder cannot be created
     */
    protected void setUp() throws Exception {
        CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
        FileUtils.cleanDirectory(GEN_SRC_DIR);
        CodeGenUtil.checkDirectory(GEN_XJB_DIR, true);
        FileUtils.cleanDirectory(GEN_XJB_DIR);
        DocumentBuilderFactory docFac = DocumentBuilderFactory.newInstance();
        docFac.setNamespaceAware(true);
        _db = docFac.newDocumentBuilder();
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

    /**
     * Reads a complete source file into a string.
     * 
     * @param schemaName the schema used to generate
     * @param className the generated class name
     * @return a String with the class content
     */
    protected String getSource(final String schemaName, final String className) {
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
    protected String getSource(final String schemaName, final String srcSubDir,
            final String className) {
        File srcFile = new File(GEN_SRC_DIR, srcSubDir + '/' + schemaName + '/'
                + className + ".java");
        return getSource(srcFile);
    }

    /**
     * Reads a complete source file into a string.
     * 
     * @param srcFile the source file to read into a string
     * @return a String with the class content
     */
    protected String getSource(final File srcFile) {
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
    protected XmlSchema getXmlSchema(final String source) {
        XmlSchemaCollection schemaCol = new XmlSchemaCollection();
        return schemaCol.read(new StringReader(source), null);
    }

    /**
     * Returns the XML schema contant in a string.
     * 
     * @param xsd the XML Schema
     * @return a string holding the schema content
     */
    protected String toString(final XmlSchema xsd) {
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
