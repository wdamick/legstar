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

import junit.framework.TestCase;
import com.legstar.cixs.gen.CixsServiceDescriptorFile;
import com.legstar.cixs.gen.CixsException;
import com.legstar.cixs.gen.CixsService;
import com.legstar.cixs.gen.CixsOperation;
import com.legstar.cixs.gen.CixsStructure;

import java.util.ArrayList;

public class CixsDescriptorsTest extends TestCase {
	
	
	public void testServiceGenerationNoOp() {
		CixsService sv = new CixsService();
		sv.setName("rasePoil1");
		sv.setEndpointPackageName("com.legstar.rase");
		sv.setTargetNamespace("http://rase.legstar.com");
		
		try {
			assertEquals("<cixsService name=\"rasePoil1\" endpointPackageName=\"com.legstar.rase\" targetNamespace=\"http://rase.legstar.com\"></cixsService>",
					generateDescriptor(sv));
		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
	}

	public void testServiceGenerationOneOpInEqOut() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setName("rasePoil2");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setName("coiffe");
			op1.setCicsProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc");
			
			sv.getCixsOperations().add(op1);
			
			assertEquals("<cixsService name=\"rasePoil2\" endpointPackageName=\"com.legstar.rase\" targetNamespace=\"http://rase.legstar.com\"><cixsOperation name=\"coiffe\" cicsProgramName=\"COIFFE\"><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><output jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/></cixsOperation></cixsService>",
					generateDescriptor(sv));

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
		
	}
	public void testServiceGenerationOneOpInDiffOut() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setName("rasePoil3");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setName("coiffe");
			op1.setCicsProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output");
			
			sv.getCixsOperations().add(op1);

			assertEquals("<cixsService name=\"rasePoil3\" endpointPackageName=\"com.legstar.rase\" targetNamespace=\"http://rase.legstar.com\"><cixsOperation name=\"coiffe\" cicsProgramName=\"COIFFE\"><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><output jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc.output\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/></cixsOperation></cixsService>",
					generateDescriptor(sv));

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
		
	}
	public void testServiceGenerationTwoOps() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			CixsOperation op2 = new CixsOperation();
			
			sv.setName("rasePoil4");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setName("coiffe");
			op1.setCicsProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output");
			
			sv.getCixsOperations().add(op1);
			
			op2.setName("bigoudis");
			op2.setCicsProgramName("BIGOUDIS");
			addInputStructure(op2, "DfhCommareaType", "com.tata.truc");
			addOutputStructure(op2, "DfhCommareaType", "com.tata.truc.output");
			
			sv.getCixsOperations().add(op2);
			
			assertEquals("<cixsService name=\"rasePoil4\" endpointPackageName=\"com.legstar.rase\" targetNamespace=\"http://rase.legstar.com\"><cixsOperation name=\"coiffe\" cicsProgramName=\"COIFFE\"><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><output jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc.output\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/></cixsOperation><cixsOperation name=\"bigoudis\" cicsProgramName=\"BIGOUDIS\"><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.tata.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><output jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.tata.truc.output\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/></cixsOperation></cixsService>",
					generateDescriptor(sv));

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
		
	}

	public void testServiceInputChoiceStrategy() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setName("rasePoil5");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setName("coiffe");
			op1.setCicsProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output");
			
			sv.getCixsOperations().add(op1);
			
			assertEquals("<cixsService name=\"rasePoil5\" endpointPackageName=\"com.legstar.rase\" targetNamespace=\"http://rase.legstar.com\"><cixsOperation name=\"coiffe\" cicsProgramName=\"COIFFE\"><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><output jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc.output\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/></cixsOperation></cixsService>",
					generateDescriptor(sv));

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
		
	}

	public void testServiceOutputChoiceStrategy() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setName("rasePoil5");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setName("coiffe");
			op1.setCicsProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output");
			
			sv.getCixsOperations().add(op1);
			
			assertEquals("<cixsService name=\"rasePoil5\" endpointPackageName=\"com.legstar.rase\" targetNamespace=\"http://rase.legstar.com\"><cixsOperation name=\"coiffe\" cicsProgramName=\"COIFFE\"><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><output jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc.output\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/></cixsOperation></cixsService>",
					generateDescriptor(sv));

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
	}
	
	public void testBindVarNames() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setName("rasePoil2");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setName("coiffe");
			op1.setCicsProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addInputStructure(op1, "DfhCommareaType", "com.tata.truc");
			addInputStructure(op1, "DfhCommareaType", "com.tutu.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc");
			
			sv.getCixsOperations().add(op1);
			
			assertEquals("<cixsService name=\"rasePoil2\" endpointPackageName=\"com.legstar.rase\" targetNamespace=\"http://rase.legstar.com\"><cixsOperation name=\"coiffe\" cicsProgramName=\"COIFFE\"><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.tata.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><input jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.tutu.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/><output jaxbType=\"DfhCommareaType\" jaxbPackageName=\"com.toto.truc\" jaxbPropertyName=\"DfhCommarea\" jaxbFieldName=\"dfhCommarea\"/></cixsOperation></cixsService>",
					generateDescriptor(sv));

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
	}
	
	private String generateDescriptor(CixsService sv) throws CixsException {
		CixsServiceDescriptorFile sd = new CixsServiceDescriptorFile(sv);
		return sd.getContentAsString();
	}
	
	private void addInputStructure(CixsOperation op, String jaxbType, String jaxbPackageName) {
		if (op.getInput() == null) {
			op.setInput(new ArrayList < CixsStructure >());
		}
		CixsStructure struct = new CixsStructure();
		struct.setJaxbType(jaxbType);
		struct.setJaxbPackageName(jaxbPackageName);
		op.getInput().add(struct);
	}

	private void addOutputStructure(CixsOperation op, String jaxbType, String jaxbPackageName) {
		if (op.getOutput() == null) {
			op.setOutput(new ArrayList < CixsStructure >());
		}
		CixsStructure struct = new CixsStructure();
		struct.setJaxbType(jaxbType);
		struct.setJaxbPackageName(jaxbPackageName);
		op.getOutput().add(struct);
	}
	
	
}
