/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.cixs.gen.test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import com.legstar.cixs.gen.CixsException;
import com.legstar.cixs.gen.CixsService;

import junit.framework.TestCase;

public class CixsServiceTest extends TestCase {
	
	/** Pattern for temporary files. */
	private static final String TEMP_PATTERN = "legstar";
	
	/** Suffix for temporary files. */
	private static final String TEMP_SUFFIX = ".cixs";
	
	public void testLoadFromEmptyFile() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        CixsService service = new CixsService();
        try {
			service.load(temp);
		} catch (CixsException e) {
			assertEquals("org.xml.sax.SAXParseException: Premature end of file.", e.getMessage());
		}
	}

	public void testLoadFromFileWrongContent() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsOperation></cixsOperation>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			
		} catch (CixsException e) {
			assertEquals("Empty or invalid service descriptor file", e.getMessage());
		}
	}
	
	public void testLoadFromServiceWithNoName() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsService></cixsService>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			fail("testLoadFromServiceWithNoName");
		} catch (CixsException e) {
			assertEquals("Service must have a name", e.getMessage());
		}
	}

	public void testLoadFromFileWithNoOperations() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsService name=\"z34t\" endpointPackageName=\"pack35\" targetNamespace=\"ns45\"></cixsService>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			assertEquals("z34t", service.getName());
			assertEquals("pack35", service.getEndpointPackageName());
			assertEquals("ns45", service.getTargetNamespace());
			
		} catch (CixsException e) {
			fail("testLoadFromFileWithNoOperations");
		}
	}

	public void testLoadFromFileWithUnnamedOperation() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsService name=\"z34t\" endpointPackageName=\"pack35\" targetNamespace=\"ns45\"><cixsOperation></cixsOperation></cixsService>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			fail("testLoadFromFileWithUnnamedOperation");
			
		} catch (CixsException e) {
			assertEquals("Operation must have a name", e.getMessage());
		}
	}

	public void testLoadFromFileWithOperationWithoutProgram() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsService name=\"z34t\" endpointPackageName=\"pack35\" targetNamespace=\"ns45\"><cixsOperation name=\"toz\"></cixsOperation></cixsService>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			fail("testLoadFromFileWithOperationWithoutProgram");
			
		} catch (CixsException e) {
			assertEquals("Operation must have an associated program name", e.getMessage());
		}
	}

	public void testLoadFromFileWith1OperationNoInputNoOutput() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsService name=\"z34t\" endpointPackageName=\"pack35\" targetNamespace=\"ns45\"><cixsOperation name=\"toz\" cicsProgramName=\"prog\" cicsChannel=\"chan\"></cixsOperation></cixsService>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			assertEquals("toz", service.getCixsOperations().get(0).getName());
			assertEquals("prog", service.getCixsOperations().get(0).getCicsProgramName());
			assertEquals("chan", service.getCixsOperations().get(0).getCicsChannel());
		} catch (CixsException e) {
			fail("testLoadFromFileWith1Operation");
		}
	}

	public void testLoadFromFileWith1Operation1input() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsService name=\"z34t\" endpointPackageName=\"pack35\" targetNamespace=\"ns45\">");
        out.write("<cixsOperation name=\"toz\" cicsProgramName=\"prog\" cicsChannel=\"chan\">");
        out.write("<input jaxbType=\"jType\" jaxbPackageName=\"jPack\" cicsContainer=\"cont\"/>");
        out.write("</cixsOperation>");
        out.write("</cixsService>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			assertEquals("toz", service.getCixsOperations().get(0).getName());
			assertEquals("prog", service.getCixsOperations().get(0).getCicsProgramName());
			assertEquals("chan", service.getCixsOperations().get(0).getCicsChannel());
			assertEquals("jType", service.getCixsOperations().get(0).getInput().get(0).getJaxbType());
			assertEquals("jPack", service.getCixsOperations().get(0).getInput().get(0).getJaxbPackageName());
			assertEquals("cont", service.getCixsOperations().get(0).getInput().get(0).getCicsContainer());
		} catch (CixsException e) {
			fail("testLoadFromFileWith1Operation1input");
		}
	}

	public void testLoadFromFileWith2Operations2inputs2outputs() throws IOException {
		File temp;
        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
        temp.deleteOnExit();
        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
        out.write("<cixsService name=\"z34t\" endpointPackageName=\"pack35\" targetNamespace=\"ns45\">");
        out.write("<cixsOperation name=\"toz1\" cicsProgramName=\"prog1\" cicsChannel=\"chan1\">");
        out.write("<input jaxbType=\"jTypei11\" jaxbPackageName=\"jPacki11\" cicsContainer=\"conti11\"/>");
        out.write("<input jaxbType=\"jTypei12\" jaxbPackageName=\"jPacki12\" cicsContainer=\"conti12\"/>");
        out.write("<output jaxbType=\"jTypeo11\" jaxbPackageName=\"jPacko11\" cicsContainer=\"conto11\"/>");
        out.write("<output jaxbType=\"jTypeo12\" jaxbPackageName=\"jPacko12\" cicsContainer=\"conto12\"/>");
        out.write("</cixsOperation>");
        out.write("<cixsOperation name=\"toz2\" cicsProgramName=\"prog2\" cicsChannel=\"chan2\">");
        out.write("<input jaxbType=\"jTypei21\" jaxbPackageName=\"jPacki21\" cicsContainer=\"conti21\"/>");
        out.write("<input jaxbType=\"jTypei22\" jaxbPackageName=\"jPacki22\" cicsContainer=\"conti22\"/>");
        out.write("<output jaxbType=\"jTypeo21\" jaxbPackageName=\"jPacko21\" cicsContainer=\"conto21\"/>");
        out.write("<output jaxbType=\"jTypeo22\" jaxbPackageName=\"jPacko22\" cicsContainer=\"conto22\"/>");
        out.write("</cixsOperation>");
        out.write("</cixsService>");
        out.close();
        CixsService service = new CixsService();
        try {
			service.load(temp);
			assertEquals("toz1", service.getCixsOperations().get(0).getName());
			assertEquals("prog1", service.getCixsOperations().get(0).getCicsProgramName());
			assertEquals("chan1", service.getCixsOperations().get(0).getCicsChannel());
			assertEquals("jTypei11", service.getCixsOperations().get(0).getInput().get(0).getJaxbType());
			assertEquals("jPacki11", service.getCixsOperations().get(0).getInput().get(0).getJaxbPackageName());
			assertEquals("conti11", service.getCixsOperations().get(0).getInput().get(0).getCicsContainer());
			assertEquals("jTypei12", service.getCixsOperations().get(0).getInput().get(1).getJaxbType());
			assertEquals("jPacki12", service.getCixsOperations().get(0).getInput().get(1).getJaxbPackageName());
			assertEquals("conti12", service.getCixsOperations().get(0).getInput().get(1).getCicsContainer());
			assertEquals("jTypeo11", service.getCixsOperations().get(0).getOutput().get(0).getJaxbType());
			assertEquals("jPacko11", service.getCixsOperations().get(0).getOutput().get(0).getJaxbPackageName());
			assertEquals("conto11", service.getCixsOperations().get(0).getOutput().get(0).getCicsContainer());
			assertEquals("jTypeo12", service.getCixsOperations().get(0).getOutput().get(1).getJaxbType());
			assertEquals("jPacko12", service.getCixsOperations().get(0).getOutput().get(1).getJaxbPackageName());
			assertEquals("conto12", service.getCixsOperations().get(0).getOutput().get(1).getCicsContainer());

			assertEquals("toz2", service.getCixsOperations().get(1).getName());
			assertEquals("prog2", service.getCixsOperations().get(1).getCicsProgramName());
			assertEquals("chan2", service.getCixsOperations().get(1).getCicsChannel());
			assertEquals("jTypei21", service.getCixsOperations().get(1).getInput().get(0).getJaxbType());
			assertEquals("jPacki21", service.getCixsOperations().get(1).getInput().get(0).getJaxbPackageName());
			assertEquals("conti21", service.getCixsOperations().get(1).getInput().get(0).getCicsContainer());
			assertEquals("jTypei22", service.getCixsOperations().get(1).getInput().get(1).getJaxbType());
			assertEquals("jPacki22", service.getCixsOperations().get(1).getInput().get(1).getJaxbPackageName());
			assertEquals("conti22", service.getCixsOperations().get(1).getInput().get(1).getCicsContainer());
			assertEquals("jTypeo21", service.getCixsOperations().get(1).getOutput().get(0).getJaxbType());
			assertEquals("jPacko21", service.getCixsOperations().get(1).getOutput().get(0).getJaxbPackageName());
			assertEquals("conto21", service.getCixsOperations().get(1).getOutput().get(0).getCicsContainer());
			assertEquals("jTypeo22", service.getCixsOperations().get(1).getOutput().get(1).getJaxbType());
			assertEquals("jPacko22", service.getCixsOperations().get(1).getOutput().get(1).getJaxbPackageName());
			assertEquals("conto22", service.getCixsOperations().get(1).getOutput().get(1).getCicsContainer());
		} catch (CixsException e) {
			fail("testLoadFromFileWith2Operations2inputs2outputs");
		}
	}
	public void testSerializeCommarea() {
		CixsService sv = CixsGenerationTest.setCommareaService("lsfileae");
		assertEquals("<cixsService name=\"lsfileae\" endpointPackageName=\"com.legstar.test.cixs.lsfileae\" targetNamespace=\"http://cixs.test.legstar.com/lsfileae\"><cixsOperation name=\"lsfileae\" cicsProgramName=\"LSFILEAE\"><input jaxbType=\"DfhcommareaType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileae\" jaxbPropertyName=\"Dfhcommarea\" jaxbFieldName=\"dfhcommarea\"/><output jaxbType=\"DfhcommareaType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileae\" jaxbPropertyName=\"Dfhcommarea\" jaxbFieldName=\"dfhcommarea\"/></cixsOperation></cixsService>",
				sv.serialize().replaceAll("\r\n", ""));
	}
	
	public void testSerializeContainers() {
		CixsService sv = CixsGenerationTest.setContainerService();
		assertEquals("<cixsService name=\"lsfileac\" endpointPackageName=\"com.legstar.test.cixs.lsfileac\" targetNamespace=\"http://cixs.test.legstar.com/lsfileac\"><cixsOperation name=\"lsfileac\" cicsProgramName=\"LSFILEAC\" cicsChannel=\"LSFILEAC-CHANNEL\"><input jaxbType=\"QueryDataType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileac\" cicsContainer=\"QueryData\" jaxbPropertyName=\"QueryData\" jaxbFieldName=\"queryData\"/><input jaxbType=\"QueryLimitType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileac\" cicsContainer=\"QueryLimit\" jaxbPropertyName=\"QueryLimit\" jaxbFieldName=\"queryLimit\"/><output jaxbType=\"ReplyDataType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileac\" cicsContainer=\"ReplyData\" jaxbPropertyName=\"ReplyData\" jaxbFieldName=\"replyData\"/><output jaxbType=\"ReplyStatusType\" jaxbPackageName=\"com.legstar.test.coxb.lsfileac\" cicsContainer=\"ReplyStatus\" jaxbPropertyName=\"ReplyStatus\" jaxbFieldName=\"replyStatus\"/></cixsOperation></cixsService>",
			sv.serialize().replaceAll("\r\n", ""));
	}
	
}
