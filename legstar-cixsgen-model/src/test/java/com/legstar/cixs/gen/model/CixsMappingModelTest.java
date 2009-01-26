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
package com.legstar.cixs.gen.model;

import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import junit.framework.TestCase;

/**
 * Test the CixsMappingModel class.
 *
 */
public class CixsMappingModelTest extends TestCase {

    /**
     * The usual binding package prefix.
     */
    private static final String COXB_PACKAGE_PREFIX = "com.legstar.test.coxb.";

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(CixsMappingModelTest.class);


    /** A sample serialized model.*/
    private static final String SERIALIZED_MODEL =
        "<cixsMapping name=\"myName\" packageName=\"com.legstar.my.package\">"
        + "<cixsOperation name=\"lsfileal\" cicsProgramName=\"LSFILEAL\"> "
        + "<input jaxbType=\"RequestParmsType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />"
        + "<output jaxbType=\"ReplyDataType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />"
        + "</cixsOperation>"
        + "</cixsMapping>";

    /** A sample serialized compatible model.*/
    private static final String SERIALIZED_COMPATIBLE_MODEL =
        "<cixsJaxwsService name=\"myName\" packageName=\"com.legstar.my.package\">"
        + "<cixsOperation name=\"lsfileal\" cicsProgramName=\"LSFILEAL\"> "
        + "<input jaxbType=\"RequestParmsType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />"
        + "<output jaxbType=\"ReplyDataType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />"
        + "</cixsOperation>"
        + "</cixsJaxwsService>";


    /**
     * Serialize the mapping.
     * @throws CixsModelException if CixsModel cannot be built.
     */
    public void testSerialize() throws CixsModelException {
        CixsMappingModel model = new CixsMappingModel();
        model.setName("myName");
        model.addCixsOperation(getNewCommareaOperation("lsfileal", "RequestParmsType", "ReplyDataType"));

        String result = model.serialize();
        LOG.debug(result);
        assertTrue(result.contains("<cixsMapping name=\"myName\">"));
        assertTrue(result.contains("<cixsOperation name=\"lsfileal\" cicsProgramName=\"LSFILEAL\" "));
        assertTrue(result.contains("<input jaxbType=\"RequestParmsType\""
                + " jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" "));
        assertTrue(result.contains("<output jaxbType=\"ReplyDataType\""
                + " jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" "));
    }

    /**
     * Restore a serialized model.
     * @throws CixsModelException if load fails
     */
    public void testLoad() throws CixsModelException {
        CixsMappingModel model = new CixsMappingModel();
        model.load(SERIALIZED_MODEL);
        assertEquals("myName", model.getName());
        assertEquals("lsfileal", model.getCixsOperations().get(0).getName());
        assertEquals("LSFILEAL", model.getCixsOperations().get(0).getCicsProgramName());
        assertEquals("RequestParmsType",
                model.getCixsOperations().get(0).getInput().get(0).getJaxbType());
        assertEquals("com.legstar.test.coxb.lsfileal",
                model.getCixsOperations().get(0).getInput().get(0).getJaxbPackageName());
    }

    /**
     * Test that the wrapper element does not matter. Only the content.
     * @throws CixsModelException if load fails
     */
    public void testLoadCompat() throws CixsModelException {
        CixsMappingModel model = new CixsMappingModel();
        model.load(SERIALIZED_COMPATIBLE_MODEL);
        assertEquals("myName", model.getName());
        assertEquals("lsfileal", model.getCixsOperations().get(0).getName());
        assertEquals("LSFILEAL", model.getCixsOperations().get(0).getCicsProgramName());
        assertEquals("RequestParmsType",
                model.getCixsOperations().get(0).getInput().get(0).getJaxbType());
        assertEquals("com.legstar.test.coxb.lsfileal",
                model.getCixsOperations().get(0).getInput().get(0).getJaxbPackageName());
    }

    /**
     * Built a CixsOperation.
     * @param name operation name
     * @param inputJaxbType input JAXB type
     * @param outputJaxbType output JAXB type
     * @return a CixsOperation
     */
    private static CixsOperation getNewCommareaOperation(
            final String name, final String inputJaxbType, final String outputJaxbType) {
        CixsOperation operation = new CixsOperation();
        operation.setInput(new ArrayList < CixsStructure >());
        operation.setOutput(new ArrayList < CixsStructure >());
        CixsStructure inStruct = new CixsStructure();
        CixsStructure outStruct = new CixsStructure();

        operation.setName(name);
        operation.setCicsProgramName(name.toUpperCase());
        inStruct.setJaxbType(inputJaxbType);
        inStruct.setJaxbPackageName(COXB_PACKAGE_PREFIX + name);
        outStruct.setJaxbType(outputJaxbType);
        outStruct.setJaxbPackageName(inStruct.getJaxbPackageName());

        operation.getInput().add(inStruct);
        operation.getOutput().add(outStruct);

        return operation;
    }

}
