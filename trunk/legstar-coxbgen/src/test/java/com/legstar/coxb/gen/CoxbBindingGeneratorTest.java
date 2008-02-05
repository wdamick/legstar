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
package com.legstar.coxb.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import junit.framework.TestCase;
import com.legstar.coxb.gen.CoxbBindingGenerator;

public class CoxbBindingGeneratorTest extends TestCase {
	
	private static final boolean DEBUG_MODE = false;
	private static final String GEN_SRC_DIR = "src/test/gen/java";
	private static final String GEN_SRC_SUBDIR = "com/legstar/test/coxb";
	private static final String GEN_CUST_SUBDIR = "com/legstar/coxb/cust";

	/** Generated JAXB classes binaries. */
	private static final String JAXB_DIR = "../legstar-jaxbgen-cases/target/classes";
	/** Generated JAXB classes pavkage prefix. */
	private static final String JAXB_PKG_PFX = "com.legstar.test.coxb";
	
	
	/** Make sure we have an output folder. */
	protected void setUp() throws Exception {
		java.io.File td = new java.io.File(GEN_SRC_DIR);
		td.mkdirs();
	}
	
	/** Generator should check on package name. */
	public void testCheckOnPackageName() {
		try {
			CoxbBindingGenerator gen = new CoxbBindingGenerator();
			gen.execute();
		} catch (RuntimeException e) {
			assertEquals("You must specify either a JAXB package name or an XML schema file name", e.getMessage());
		}
	}
	
	/** Generator should check on target directory. */
	public void testCheckOnTarget() {
		try {
			CoxbBindingGenerator gen = new CoxbBindingGenerator();
			gen.setJaxbPackageName("com.legstar.test.coxb.vararcom");
			gen.execute();
		} catch (RuntimeException e) {
			assertEquals("You must specify a target directory", e.getMessage());
		}
	}
	
	/** Generator should check on JAXB root name. */
	public void testCheckOnJaxbRoot() {
		try {
			CoxbBindingGenerator gen = new CoxbBindingGenerator();
			gen.setJaxbPackageName("com.legstar.test.coxb.vararcom");
			gen.setTargetDir(new File(GEN_SRC_DIR));
			gen.execute();
		} catch (RuntimeException e) {
			assertEquals("You must specify a JAXB root object name", e.getMessage());
		}
	}
	
	/** Generate binding for Alltypes. */
	public void testGenAlltypes() throws Exception  {
		genSource("alltypes", "DfhcommareaType");
		String srce = getSource(getGetSrcFilename("alltypes", "DfhcommareaType"));
		assertTrue(srce.contains("sString = BF.createStringBinding(\"SString\","));
		assertTrue(srce.contains("\"SString\", String.class, this);"));
	}

	/** Generate binding for Dplarcht. */
	public void testGenDplarcht() throws Exception   {
		genSource("dplarcht", "DfhcommareaType");
		String srce = getSource(getGetSrcFilename("dplarcht", "DfhcommareaType"));
		assertTrue(srce.contains("lsRequest = new LsRequestTypeBinding(\"LsRequest\","));
		assertTrue(srce.contains("\"LsRequest\", this, null);"));
		assertTrue(srce.contains("lsReply = new LsReplyTypeBinding(\"LsReply\","));
		assertTrue(srce.contains("\"LsReply\", this, null);"));
	}
	
	/** Generate binding for Redsimpt. */
	public void testGenRedsimpt() throws Exception   {
		File custFile = new File(getGetCustFilename("redsimpt"));
		custFile.delete();
		genSource("redsimpt", "DfhcommareaType");
		String srce = getSource(getGetSrcFilename("redsimpt", "DfhcommareaType"));
		assertTrue(srce.contains("cDefinition1Choice = new CDefinition1ChoiceBinding(\"CDefinition1Choice\", this);"));
		assertTrue(srce.contains("cDefinition1Choice.setUnmarshalChoiceStrategyClassName("));
		assertTrue(srce.contains("\"com.legstar.coxb.cust.redsimpt.ChoiceSelector\");"));
		String custSrce = getSource(getGetCustFilename("redsimpt"));
		assertTrue(custSrce.contains("DfhcommareaType jaxbo = (DfhcommareaType) choice.getObjectValue(DfhcommareaType.class);"));
	}
	
	/** Generate binding for Arrayssm. */
	public void testGenArrayssm() throws Exception   {
		genSource("arrayssm", "DfhcommareaType");
		String srce = getSource(getGetSrcFilename("arrayssm", "DfhcommareaType"));
		assertTrue(srce.contains("tableComplexWrapperItem = new TableComplexTypeBinding(\"TableComplexWrapperItem\","));
		assertTrue(srce.contains("\"TableComplex\", this, null);"));
		assertTrue(srce.contains("tableComplexWrapper = new TableComplexTypeWrapperBinding(\"TableComplexWrapper\","));
		assertTrue(srce.contains("\"TableComplex\", this, tableComplexWrapperItem);"));
	}
	
	/** Generate binding for Redsimpt. */
	public void testGenLsfileae() throws Exception   {
		genSource("lsfileae", "DfhcommareaType");
		String srce = getSource(getGetSrcFilename("lsfileae", "DfhcommareaType"));
		assertTrue(srce.contains("comNumber = BF.createZonedDecimalBinding(\"ComNumber\","));
		assertTrue(srce.contains("\"ComNumber\", Long.class, this);"));
		}
	
	/** Generate binding for Arraysdo. */
	public void testGenArraysdo() throws Exception   {
		genSource("arraysdo", "DfhcommareaType");
		String srce = getSource(getGetSrcFilename("arraysdo", "DfhcommareaType"));
		assertTrue(srce.contains("tableOdo = BF.createArrayStringBinding(\"TableOdo\","));
		assertTrue(srce.contains("\"TableOdo\", String.class, this);"));
		assertTrue(srce.contains("tableOdo.setByteLength(5);"));
		assertTrue(srce.contains("tableOdo.setCobolName(\"TABLE-ODO\");"));
		assertTrue(srce.contains("tableOdo.setMinOccurs(1);"));
		assertTrue(srce.contains("tableOdo.setMaxOccurs(100);"));
		assertTrue(srce.contains("tableOdo.setDependingOn(\"TABLE-SIZE\");"));
		}
	
	/** Generate binding for Listssdo. */
	public void testGenListssdo() throws Exception   {
		genSource("listssdo", "DfhcommareaType");
		String srce = getSource(getGetSrcFilename("listssdo", "DfhcommareaType"));
		assertTrue(srce.contains("listOdoCounter = BF.createBinaryBinding(\"ListOdoCounter\","));
		assertTrue(srce.contains("this);"));
		assertTrue(srce.contains("listOdoCounter.setByteLength(4);"));
		assertTrue(srce.contains("listOdoCounter.setCobolName(\"LIST-ODO--C\");"));
		assertTrue(srce.contains("listOdoCounter.setTotalDigits(9);"));
		assertTrue(srce.contains("listOdoCounter.setIsODOObject(true);"));
		assertTrue(srce.contains("listOdo = BF.createArrayStringBinding(\"ListOdo\","));
		assertTrue(srce.contains("\"ListOdo\", String.class, this);"));
		}
	
	/** Generate binding for MSNSearch SearchRequestType. */
	public void testMSNSearchSearchRequestType() throws Exception   {
		genSource("MSNSearch", "SearchRequestType");
		String srce = getSource(getGetSrcFilename("MSNSearch", "SearchRequestType"));
		assertTrue(srce.contains("flagsCounter = BF.createBinaryBinding(\"FlagsCounter\","));
		assertTrue(srce.contains("this);"));
		assertTrue(srce.contains("flagsCounter.setByteLength(4);"));
		assertTrue(srce.contains("flagsCounter.setCobolName(\"Flags--C\");"));
		assertTrue(srce.contains("flagsCounter.setTotalDigits(9);"));
		assertTrue(srce.contains("flagsCounter.setIsODOObject(true);"));
		assertTrue(srce.contains("safeSearch = BF.createStringBinding(\"SafeSearch\","));
		assertTrue(srce.contains("\"SafeSearch\", SafeSearchOptionsType.class, this);"));
		}
	
	/** Generate binding for MSNSearch SearchResponseType. */
	public void testMSNSearchSearchResponseType() throws Exception   {
		genSource("MSNSearch", "SearchResponseType");
		String srce = getSource(getGetSrcFilename("MSNSearch", "SourceResponseType"));
		assertTrue(srce.contains("source = BF.createStringBinding(\"Source\","));
		assertTrue(srce.contains("\"Source\", SourceTypeType.class, this);"));
		assertTrue(srce.contains("source.setByteLength(32);"));
		assertTrue(srce.contains("source.setCobolName(\"R-Source\");"));
		assertTrue(srce.contains("results = new ArrayOfResultResultsTypeBinding(\"Results\","));
		assertTrue(srce.contains("\"Results\", this, null);"));
		}
	
	/** Generates COXB classes */
	private void genSource(String schemaName, String rootName) {
		CoxbBindingGenerator gen = new CoxbBindingGenerator();
		gen.setJaxbDir(new File(JAXB_DIR));
		gen.setJaxbPackageName(JAXB_PKG_PFX + '.' + schemaName);
		gen.setJaxbRootObjectName(rootName);
		gen.setTargetDir(new File(GEN_SRC_DIR));
		gen.execute();
	}
	
	private String getGetSrcFilename(String schemaName, String className) {
        return GEN_SRC_DIR + '/' + GEN_SRC_SUBDIR + '/' + schemaName + "/bind/" + className + "Binding.java";
	}
	
	private String getGetCustFilename(String schemaName) {
        return GEN_SRC_DIR + '/' + GEN_CUST_SUBDIR + '/' + schemaName + "/ChoiceSelector.java";
	}
	
	/** Reads a complete source file into a string. */
	private String getSource(String srcFileName) {
		/* Read the resulting output source*/
	    try {
	    	BufferedReader in = new BufferedReader(new FileReader(srcFileName));
	        StringBuffer resStr = new StringBuffer();
	        String str = in.readLine();
	        while (str != null) {
	        	if (DEBUG_MODE) {
	        		System.out.println(str);
	        	}
	        	resStr.append(str);
	        	str = in.readLine();
	        }
	        in.close();
			return resStr.toString();
	    } catch (IOException e) {
			fail("Source file " + srcFileName + " was not generated");
			return null;
	    }
	}
}
