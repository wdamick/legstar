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
import com.legstar.cixs.gen.CixsBaseDescriptors;
import com.legstar.cixs.gen.CixsException;
import com.legstar.cixs.gen.CixsService;
import com.legstar.cixs.gen.CixsOperation;
import com.legstar.cixs.gen.CixsStructure;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class CixsDescriptorsTest extends TestCase {
	
	
	public void testServiceGenerationNoOp() {
		CixsService sv = new CixsService();
		sv.setServiceName("rasePoil1");
		sv.setEndpointPackageName("com.legstar.rase");
		sv.setTargetNamespace("http://rase.legstar.com");
		
		try {
			CixsBaseDescriptors cd = new CixsBaseDescriptors();
			java.io.File f = cd.getTempFile();
			cd.createServiceContent(sv, f);
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(f));
		        String str = in.readLine();
		        in.close();
				assertEquals("<cixs-service><service-name>rasePoil1</service-name><service-endpoint-package>com.legstar.rase</service-endpoint-package><service-targetnamespace>http://rase.legstar.com</service-targetnamespace></cixs-service>", str);
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
	}

	public void testServiceGenerationOneOpInEqOut() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setServiceName("rasePoil2");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setOperationName("coiffe");
			op1.setProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc", "");
			
			sv.getOperations().add(op1);
			
			CixsBaseDescriptors cd = new CixsBaseDescriptors();
			java.io.File f = cd.getTempFile();
			cd.createServiceContent(sv, f);
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(f));
		        String str = in.readLine();
				assertEquals("<cixs-service><service-name>rasePoil2</service-name><service-endpoint-package>com.legstar.rase</service-endpoint-package><service-targetnamespace>http://rase.legstar.com</service-targetnamespace><cixs-operation><operation-name>coiffe</operation-name><program-name>COIFFE</program-name><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea\" bind-field-name=\"inputDfhCommarea\"/><output jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"OutputDfhCommarea\" bind-field-name=\"outputDfhCommarea\"/></cixs-operation></cixs-service>", str);
		        in.close();
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
		
	}
	public void testServiceGenerationOneOpInDiffOut() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setServiceName("rasePoil3");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setOperationName("coiffe");
			op1.setProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output", "");
			
			sv.getOperations().add(op1);

			CixsBaseDescriptors cd = new CixsBaseDescriptors();
			java.io.File f = cd.getTempFile();
			cd.createServiceContent(sv, f);
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(f));
		        String str = in.readLine();
				assertEquals("<cixs-service><service-name>rasePoil3</service-name><service-endpoint-package>com.legstar.rase</service-endpoint-package><service-targetnamespace>http://rase.legstar.com</service-targetnamespace><cixs-operation><operation-name>coiffe</operation-name><program-name>COIFFE</program-name><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea\" bind-field-name=\"inputDfhCommarea\"/><output jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc.output\" jaxb-classes-location=\"com/toto/truc/output\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"OutputDfhCommarea\" bind-field-name=\"outputDfhCommarea\"/></cixs-operation></cixs-service>", str);
		        in.close();
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }

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
			
			sv.setServiceName("rasePoil4");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setOperationName("coiffe");
			op1.setProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output", "");
			
			sv.getOperations().add(op1);
			
			op2.setOperationName("bigoudis");
			op2.setProgramName("BIGOUDIS");
			addInputStructure(op2, "DfhCommareaType", "com.tata.truc");
			addOutputStructure(op2, "DfhCommareaType", "com.tata.truc.output", "");
			
			sv.getOperations().add(op2);
			
			CixsBaseDescriptors cd = new CixsBaseDescriptors();
			java.io.File f = cd.getTempFile();
			cd.createServiceContent(sv, f);
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(f));
		        String str = in.readLine();
				assertEquals("<cixs-service><service-name>rasePoil4</service-name><service-endpoint-package>com.legstar.rase</service-endpoint-package><service-targetnamespace>http://rase.legstar.com</service-targetnamespace><cixs-operation><operation-name>coiffe</operation-name><program-name>COIFFE</program-name><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea\" bind-field-name=\"inputDfhCommarea\"/><output jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc.output\" jaxb-classes-location=\"com/toto/truc/output\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"OutputDfhCommarea\" bind-field-name=\"outputDfhCommarea\"/></cixs-operation><cixs-operation><operation-name>bigoudis</operation-name><program-name>BIGOUDIS</program-name><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.tata.truc\" jaxb-classes-location=\"com/tata/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea0\" bind-field-name=\"inputDfhCommarea0\"/><output jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.tata.truc.output\" jaxb-classes-location=\"com/tata/truc/output\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"OutputDfhCommarea0\" bind-field-name=\"outputDfhCommarea0\"/></cixs-operation></cixs-service>", str);
		        in.close();
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
		
	}

	public void testServiceInputChoiceStrategy() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setServiceName("rasePoil5");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setOperationName("coiffe");
			op1.setProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output", "com.toto.truc.custom.ChoiceSelector");
			
			sv.getOperations().add(op1);
			
			CixsBaseDescriptors cd = new CixsBaseDescriptors();
			java.io.File f = cd.getTempFile();
			cd.createServiceContent(sv, f);
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(f));
		        String str = in.readLine();
				assertEquals("<cixs-service><service-name>rasePoil5</service-name><service-endpoint-package>com.legstar.rase</service-endpoint-package><service-targetnamespace>http://rase.legstar.com</service-targetnamespace><cixs-operation><operation-name>coiffe</operation-name><program-name>COIFFE</program-name><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea\" bind-field-name=\"inputDfhCommarea\"/><output jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc.output\" jaxb-classes-location=\"com/toto/truc/output\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"OutputDfhCommarea\" bind-field-name=\"outputDfhCommarea\" choice-strategy=\"com.toto.truc.custom.ChoiceSelector\"/></cixs-operation></cixs-service>", str);
		        in.close();
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
		
	}

	public void testServiceOutputChoiceStrategy() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setServiceName("rasePoil5");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setOperationName("coiffe");
			op1.setProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc.output", "com.toto.truc.custom.ChoiceSelector");
			
			sv.getOperations().add(op1);
			
			CixsBaseDescriptors cd = new CixsBaseDescriptors();
			java.io.File f = cd.getTempFile();
			cd.createServiceContent(sv, f);
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(f));
		        String str = in.readLine();
				assertEquals("<cixs-service><service-name>rasePoil5</service-name><service-endpoint-package>com.legstar.rase</service-endpoint-package><service-targetnamespace>http://rase.legstar.com</service-targetnamespace><cixs-operation><operation-name>coiffe</operation-name><program-name>COIFFE</program-name><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea\" bind-field-name=\"inputDfhCommarea\"/><output jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc.output\" jaxb-classes-location=\"com/toto/truc/output\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"OutputDfhCommarea\" bind-field-name=\"outputDfhCommarea\" choice-strategy=\"com.toto.truc.custom.ChoiceSelector\"/></cixs-operation></cixs-service>", str);
		        in.close();
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
	}
	
	public void testBindVarNames() {
		try {
			CixsService sv = new CixsService();
			CixsOperation op1 = new CixsOperation();
			
			sv.setServiceName("rasePoil2");
			sv.setEndpointPackageName("com.legstar.rase");
			sv.setTargetNamespace("http://rase.legstar.com");

			op1.setOperationName("coiffe");
			op1.setProgramName("COIFFE");
			addInputStructure(op1, "DfhCommareaType", "com.toto.truc");
			addInputStructure(op1, "DfhCommareaType", "com.tata.truc");
			addInputStructure(op1, "DfhCommareaType", "com.tutu.truc");
			addOutputStructure(op1, "DfhCommareaType", "com.toto.truc", "");
			
			sv.getOperations().add(op1);
			
			CixsBaseDescriptors cd = new CixsBaseDescriptors();
			java.io.File f = cd.getTempFile();
			cd.createServiceContent(sv, f);
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(f));
		        String str = in.readLine();
				assertEquals("<cixs-service><service-name>rasePoil2</service-name><service-endpoint-package>com.legstar.rase</service-endpoint-package><service-targetnamespace>http://rase.legstar.com</service-targetnamespace><cixs-operation><operation-name>coiffe</operation-name><program-name>COIFFE</program-name><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea\" bind-field-name=\"inputDfhCommarea\"/><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.tata.truc\" jaxb-classes-location=\"com/tata/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea0\" bind-field-name=\"inputDfhCommarea0\"/><input jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.tutu.truc\" jaxb-classes-location=\"com/tutu/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"InputDfhCommarea1\" bind-field-name=\"inputDfhCommarea1\"/><output jaxb-type=\"DfhCommareaType\" jaxb-package=\"com.toto.truc\" jaxb-classes-location=\"com/toto/truc\" jaxb-property-name=\"DfhCommarea\" jaxb-field-name=\"dfhCommarea\" bind-property-name=\"OutputDfhCommarea\" bind-field-name=\"outputDfhCommarea\"/></cixs-operation></cixs-service>", str);
		        in.close();
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
	}
	
	public void testPropertyNameToFieldName() {
		String fieldName = CixsBaseDescriptors.fieldNameFromPropertyName("DfhCommarea");
		assertEquals("dfhCommarea", fieldName);
		fieldName = CixsBaseDescriptors.fieldNameFromPropertyName("X");
		assertEquals("x", fieldName);
		
	}
	private void addInputStructure(CixsOperation op, String jaxbType, String jaxbPackageName) {
		if (op.getInputStructures() == null) {
			op.setInputStructures(new ArrayList < CixsStructure >());
		}
		CixsStructure struct = new CixsStructure();
		struct.setJaxbType(jaxbType);
		struct.setJaxbPackageName(jaxbPackageName);
		op.getInputStructures().add(struct);
	}

	private void addOutputStructure(CixsOperation op, String jaxbType, String jaxbPackageName, String choiceStrategy) {
		if (op.getOutputStructures() == null) {
			op.setOutputStructures(new ArrayList < CixsStructure >());
		}
		CixsStructure struct = new CixsStructure();
		struct.setJaxbType(jaxbType);
		struct.setJaxbPackageName(jaxbPackageName);
		struct.setChoiceStrategy(choiceStrategy);
		op.getOutputStructures().add(struct);
	}
	
	
}
