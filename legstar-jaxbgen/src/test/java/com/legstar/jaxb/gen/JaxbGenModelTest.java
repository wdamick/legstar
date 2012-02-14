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
package com.legstar.jaxb.gen;

import java.io.File;

import com.legstar.jaxb.AbstractJaxbGenTest;

/**
 * Test the XJB generation.
 * 
 */
public class JaxbGenModelTest extends AbstractJaxbGenTest {

    /** The generation model. */
    private JaxbGenModel _model;

    /** The generated file. */
    private File _xjbFile;

    /**
     * Create model and file.
     * 
     * @throws Exception if output folder cannot be created
     */
    public void setUp() throws Exception {
        super.setUp();
        _model = new JaxbGenModel();
        _xjbFile = new File(GEN_XJB_DIR, "bindings.xjb");
    }

    /**
     * Test the generateIsSetMethod parameter.
     * 
     * @throws Exception if something goes wrong
     */
    public void testGenerateIsSetMethod() throws Exception {

        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("generateIsSetMethod=\"true\""));

        _model.setGenerateIsSetMethod(false);
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile)
                .contains("generateIsSetMethod=\"false\""));

    }

    /**
     * Test the serializableUid parameter.
     * 
     * @throws Exception if something goes wrong
     */
    public void testGenerateSerializableUid() throws Exception {

        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("serializable uid=\"1\""));

        _model.setSerializableUid(2567889120454L);
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains(
                "serializable uid=\"2567889120454\""));

    }

    /**
     * Test the prefix/suffix parameters.
     * 
     * @throws Exception if something goes wrong
     */
    public void testPrefixesAndSuffixes() throws Exception {

        _model.setXsdLocation("test.xsd");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains(
                "bindings schemaLocation=\"test.xsd\""));

        _model.setElementNamePrefix("A");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("prefix=\"A\""));

        _model.setElementNameSuffix("B");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("prefix=\"A\""));
        assertTrue(getSource(_xjbFile).contains("suffix=\"B\""));

        _model.setTypeNamePrefix("C");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("prefix=\"C\""));

        _model.setTypeNameSuffix("D");
        _model.generateXjb(_xjbFile);
        assertTrue(getSource(_xjbFile).contains("suffix=\"D\""));
    }

}
