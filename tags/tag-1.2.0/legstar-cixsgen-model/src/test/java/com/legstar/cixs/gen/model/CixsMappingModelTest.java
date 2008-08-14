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
package com.legstar.cixs.gen.model;

import java.util.ArrayList;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;

import junit.framework.TestCase;

public class CixsMappingModelTest extends TestCase {
    
    private final static String COXB_PACKAGE_PREFIX = "com.legstar.test.coxb.";
   
    public void testSerialize() throws Exception {
        CixsMappingModel model = new CixsMappingModel();
        model.setName("myName");
        model.addCixsOperation(getNewCommareaOperation("lsfileal", "RequestParmsType", "ReplyDataType"));
        
        String result = model.serialize();
        System.out.println(result);
        assertTrue(result.contains("<cixsMapping name=\"myName\">"));
        assertTrue(result.contains("<cixsOperation name=\"lsfileal\" cicsProgramName=\"LSFILEAL\" "));
        assertTrue(result.contains("<input jaxbType=\"RequestParmsType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" "));
        assertTrue(result.contains("<output jaxbType=\"ReplyDataType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" "));
    }
    
    public void testLoad() throws Exception {
        CixsMappingModel model = new CixsMappingModel();
        model.load(getSerialized());
        assertEquals("myName", model.getName());
        assertEquals("lsfileal", model.getCixsOperations().get(0).getName());
        assertEquals("LSFILEAL", model.getCixsOperations().get(0).getCicsProgramName());
        assertEquals("RequestParmsType", model.getCixsOperations().get(0).getInput().get(0).getJaxbType());
        assertEquals("com.legstar.test.coxb.lsfileal", model.getCixsOperations().get(0).getInput().get(0).getJaxbPackageName());
    }
    
    public void testLoadCompat() throws Exception {
        CixsMappingModel model = new CixsMappingModel();
        model.load(getSerializedCompat());
        assertEquals("myName", model.getName());
        assertEquals("lsfileal", model.getCixsOperations().get(0).getName());
        assertEquals("LSFILEAL", model.getCixsOperations().get(0).getCicsProgramName());
        assertEquals("RequestParmsType", model.getCixsOperations().get(0).getInput().get(0).getJaxbType());
        assertEquals("com.legstar.test.coxb.lsfileal", model.getCixsOperations().get(0).getInput().get(0).getJaxbPackageName());
    }

    private static CixsOperation getNewCommareaOperation(String name, String inputJaxbType, String outputJaxbType) {
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
    
    private String getSerialized() {
        StringBuilder sb = new StringBuilder();
        sb.append("<cixsMapping name=\"myName\" packageName=\"com.legstar.my.package\">");
        sb.append("<cixsOperation name=\"lsfileal\" cicsProgramName=\"LSFILEAL\"> ");
        sb.append("<input jaxbType=\"RequestParmsType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />");
        sb.append("<output jaxbType=\"ReplyDataType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />");
        sb.append("</cixsOperation>");
        sb.append("</cixsMapping>");
        return sb.toString();
        
    }

    private String getSerializedCompat() {
        StringBuilder sb = new StringBuilder();
        sb.append("<cixsJaxwsService name=\"myName\" packageName=\"com.legstar.my.package\">");
        sb.append("<cixsOperation name=\"lsfileal\" cicsProgramName=\"LSFILEAL\"> ");
        sb.append("<input jaxbType=\"RequestParmsType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />");
        sb.append("<output jaxbType=\"ReplyDataType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileal\" />");
        sb.append("</cixsOperation>");
        sb.append("</cixsJaxwsService>");
        return sb.toString();
        
    }
}
