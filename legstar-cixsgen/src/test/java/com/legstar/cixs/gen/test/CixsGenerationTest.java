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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import junit.framework.TestCase;

import com.legstar.cixs.gen.CixsServiceDescriptorFile;
import com.legstar.cixs.gen.CixsException;
import com.legstar.cixs.gen.CixsEndpointSource;
import com.legstar.cixs.gen.CixsProgramProp;
import com.legstar.cixs.gen.CixsStructure;
import com.legstar.cixs.gen.CixsWebDescriptors;
import com.legstar.cixs.gen.CixsAntDeployment;
import com.legstar.cixs.gen.CixsOperation;
import com.legstar.cixs.gen.CixsService;
import com.legstar.xslt.XSLTException;


public class CixsGenerationTest extends TestCase {
	
	private static final String GEN_SRC_DIR = "src/test/java";
	private static final String GEN_WDD_DIR = "src/test/WebContent/WEB-INF";
	private static final String GEN_PROP_DIR = "src/test/WebContent/WEB-INF/classes";
	private static final String GEN_ANT_DIR = "src/test/gen-ant";
	
	private static final boolean DEBUG_MODE = false;
	
	public void testGenAlltypesSEI() throws CixsException, XSLTException {
		try {
			CixsService sv = setCommareaService("alltypes");
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			if (DEBUG_MODE) {
				try {
					printFile(f);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			
			/* Now generate endpoint code using the temporary descriptors */
			CixsEndpointSource ep = new CixsEndpointSource();
			ep.createEndpoint(f.getPath(), GEN_SRC_DIR);
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/cixs/alltypes/AlltypesImpl.java"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertTrue(resStr.contains("public class AlltypesImpl implements Alltypes"));
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}
	public void testGenDplarchtSEI() throws CixsException, XSLTException {
		try {
			CixsService sv = setCommareaService("dplarcht");
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			if (DEBUG_MODE) {
				try {
					printFile(f);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			
			/* Now generate endpoint code using the temporary descriptors */
			CixsEndpointSource ep = new CixsEndpointSource();
			ep.createEndpoint(f.getPath(), GEN_SRC_DIR);
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/cixs/dplarcht/DplarchtImpl.java"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertTrue(resStr.contains("public class DplarchtImpl implements Dplarcht"));
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}
	
	public void testGenLsfileacSEI() throws CixsException, XSLTException {
		try {
			CixsService sv = setContainerService();
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			if (DEBUG_MODE) {
				try {
					printFile(f);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			
			/* Now generate endpoint code using the temporary descriptors */
			CixsEndpointSource ep = new CixsEndpointSource();
			ep.createEndpoint(f.getPath(), GEN_SRC_DIR);
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/cixs/lsfileac/LsfileacImpl.java"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertTrue(resStr.contains("public class LsfileacImpl implements Lsfileac"));
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}

	public void testGenAlltypesProgramProp() throws CixsException, XSLTException {
		try {
			CixsService sv = setCommareaService("alltypes");
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			
			/* Now generate endpoint code using the temporary descriptors */
			CixsProgramProp pp = new CixsProgramProp();
			pp.createProgramProp(f.getPath(),  GEN_PROP_DIR);
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_PROP_DIR + "/alltypes.properties"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertEquals("# Host Program parameters# -----------------------CICSProgramName=ALLTYPESCICSLength=267CICSDataLength=267#CICSSysID#CICSSyncOnReturn#CICSTransID", resStr);
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}
	public void testGenAlltypesWebDescr() throws CixsException, XSLTException {
		try {
			CixsService sv = setCommareaService("alltypes");
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			
			/* Now generate endpoint code using the temporary descriptors */
			CixsWebDescriptors wd = new CixsWebDescriptors();
			wd.createWebDescriptors(f.getPath(), GEN_WDD_DIR + "/alltypes");
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_WDD_DIR + "/alltypes/sun-jaxws.xml"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?><endpoints xmlns=\"http://java.sun.com/xml/ns/jax-ws/ri/runtime\" version=\"2.0\">   <endpoint name=\"alltypesWebService\"             implementation=\"com.legstar.test.cixs.alltypes.AlltypesImpl\"             url-pattern=\"/alltypes\"/></endpoints>", resStr);
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}
	public void testGenDplarchtAnt() throws CixsException, XSLTException {
		try {
			CixsService sv = setCommareaService("dplarcht");
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			
			/* Now generate ant script using the temporary descriptors */
			CixsAntDeployment as = new CixsAntDeployment();
			as.createDeploymentAnt(f.getPath(),
					GEN_WDD_DIR,
					GEN_PROP_DIR,
					GEN_ANT_DIR + "/dplarcht",
					"gen-war",
					"bin",
					"bin",
					"bin",
					"bin");
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_ANT_DIR + "/dplarcht/build.xml"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?><project basedir=\"..\" default=\"create-war\" name=\"build-war\">   <property environment=\"env\"/>   <property name=\"service\" value=\"dplarcht\"/>   <target name=\"clean\">      <delete file=\"gen-war/cixs-dplarcht.war\" includeEmptyDirs=\"true\" quiet=\"true\"/>   </target>   <target name=\"create-war\" depends=\"clean\">      <war warfile=\"gen-war/cixs-dplarcht.war\"           webxml=\"src/test/WebContent/WEB-INF/web.xml\">         <webinf dir=\"src/test/WebContent/WEB-INF\" includes=\"sun-jaxws.xml\"/>         <classes dir=\"bin\">            <include name=\"com/legstar/test/coxb/dplarcht/**/*.class\"/>         </classes>         <classes dir=\"bin\">            <include name=\"com/legstar/test/coxb/dplarcht/**/*.class\"/>         </classes>         <classes dir=\"bin\">            <include name=\"**/${service}/**/*.class\"/>         </classes>         <classes dir=\"bin\">            <include name=\"**/${service}/**/*.class\"/>         </classes>         <classes dir=\"src/test/WebContent/WEB-INF/classes\">            <include name=\"dplarcht.properties\"/>         </classes>      </war>   </target></project>", resStr);
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}
	public void testGenModel() throws CixsException, XSLTException {
		/* This tests that the models of descriptors, for advanced users, are working.  */
		CixsEndpointSource ep = new CixsEndpointSource();
		ep.createEndpoint("src/xml-models/cixs-sd-lsfileae.xml", System.getProperty( "java.io.tmpdir"));
		assertTrue("no failure", true);
		
	}

	public void testGenLsfilealProgramProp() throws CixsException, XSLTException {
		try {
			CixsService sv = setCommareaService("lsfileal", "RequestParmsType", "ReplyDataType");
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			
			/* Now generate endpoint code using the temporary descriptors */
			CixsProgramProp pp = new CixsProgramProp();
			pp.createProgramProp(f.getPath(), GEN_PROP_DIR );
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_PROP_DIR + "/lsfileal.properties"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertEquals("# Host Program parameters# -----------------------CICSProgramName=LSFILEALCICSLength=8043CICSDataLength=20#CICSSysID#CICSSyncOnReturn#CICSTransID", resStr);
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}

	public void testGenLsfileacProgramProp() throws CixsException, XSLTException {
		try {
			CixsService sv = setContainerService();
			CixsServiceDescriptorFile cd = new CixsServiceDescriptorFile(sv);
			File f = cd.getServiceDescriptorFile();
			if (DEBUG_MODE) {
				try {
					printFile(f);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			
			/* Now generate endpoint code using the temporary descriptors */
			CixsProgramProp pp = new CixsProgramProp();
			pp.createProgramProp(f.getPath(), GEN_PROP_DIR );
			
			/* Read the resulting output source*/
		    try {
		        BufferedReader in = new BufferedReader(new FileReader(GEN_PROP_DIR + "/lsfileac.properties"));
		        String resStr = "";
		        String str = in.readLine();
		        while (str != null) {
		        	if (DEBUG_MODE) {
		        		System.out.println(str);
		        	}
		        	resStr += str;
		        	str = in.readLine();
		        }
		        in.close();
				assertEquals("# Host Program parameters# -----------------------CICSProgramName=LSFILEACCICSChannel=LSFILEAC-CHANNELCICSInContainers_1=QueryDataCICSInContainersLength_1=48CICSInContainers_2=QueryLimitCICSInContainersLength_2=10CICSOutContainers_1=ReplyDataCICSOutContainersLength_1=7905CICSOutContainers_2=ReplyStatusCICSOutContainersLength_2=151#CICSSysID#CICSSyncOnReturn#CICSTransID", resStr);
		    } catch (IOException e) {
				e.printStackTrace();
				fail("generation failed");
		    }
		

		} catch (CixsException e) {
			e.printStackTrace();
			fail("generation failed");
		}
			
	}
	
	/** Helper function to setup a Service in case of a single, commarea-driven
	 * operation with identical input and output layouts */
	public static CixsService setCommareaService(
			String name) {
		CixsService sv = new CixsService();
		CixsOperation op1 = new CixsOperation();
		op1.setInput(new ArrayList < CixsStructure >());
		op1.setOutput(new ArrayList < CixsStructure >());
		CixsStructure inStruct = new CixsStructure();
		CixsStructure outStruct = new CixsStructure();
		
		sv.setName(name);
		sv.setEndpointPackageName("com.legstar.test.cixs." + name);
		sv.setTargetNamespace("http://cixs.test.legstar.com/" + name);

		op1.setName(name);
		op1.setCicsProgramName(name.toUpperCase());
		inStruct.setJaxbType("DfhcommareaType");
		inStruct.setJaxbPackageName("com.legstar.test.coxb." + name);
		outStruct.setJaxbType(inStruct.getJaxbType());
		outStruct.setJaxbPackageName(inStruct.getJaxbPackageName());
		
		op1.getInput().add(inStruct);
		op1.getOutput().add(outStruct);
		
		sv.getCixsOperations().add(op1);
		return sv;
	}

	/** Helper function to setup a Service in case of a single, commarea-driven
	 * operation with different input and output layouts */
	public static CixsService setCommareaService(
			String name, String inputJaxbType, String outputJaxbType) {
		CixsService sv = new CixsService();
		CixsOperation op1 = new CixsOperation();
		op1.setInput(new ArrayList < CixsStructure >());
		op1.setOutput(new ArrayList < CixsStructure >());
		CixsStructure inStruct = new CixsStructure();
		CixsStructure outStruct = new CixsStructure();
		
		sv.setName(name);
		sv.setEndpointPackageName("com.legstar.test.cixs." + name);
		sv.setTargetNamespace("http://cixs.test.legstar.com/" + name);

		op1.setName(name);
		op1.setCicsProgramName(name.toUpperCase());
		inStruct.setJaxbType(inputJaxbType);
		inStruct.setJaxbPackageName("com.legstar.test.coxb." + name);
		outStruct.setJaxbType(outputJaxbType);
		outStruct.setJaxbPackageName(inStruct.getJaxbPackageName());
		
		op1.getInput().add(inStruct);
		op1.getOutput().add(outStruct);
		
		sv.getCixsOperations().add(op1);
		return sv;
	}

	/** Helper function to setup a Service in case of a container-driven
	 * operation with different input and output layouts */
	public static  CixsService setContainerService() {
		String name = "lsfileac";
		CixsService sv = new CixsService();
		CixsOperation op1 = new CixsOperation();
		op1.setInput(new ArrayList < CixsStructure >());
		op1.setOutput(new ArrayList < CixsStructure >());
		
		sv.setName(name);
		sv.setEndpointPackageName("com.legstar.test.cixs." + name);
		sv.setTargetNamespace("http://cixs.test.legstar.com/" + name);

		op1.setName(name);
		op1.setCicsProgramName(name.toUpperCase());
		op1.setCicsChannel(name.toUpperCase() + "-CHANNEL");

		CixsStructure inStruct = new CixsStructure();
		inStruct.setJaxbType("QueryDataType");
		inStruct.setJaxbPackageName("com.legstar.test.coxb." + name);
		inStruct.setCicsContainer("QueryData");
		op1.getInput().add(inStruct);
		
		inStruct = new CixsStructure();
		inStruct.setJaxbType("QueryLimitType");
		inStruct.setJaxbPackageName("com.legstar.test.coxb." + name);
		inStruct.setCicsContainer("QueryLimit");
		op1.getInput().add(inStruct);
		
		CixsStructure outStruct = new CixsStructure();
		outStruct.setJaxbType("ReplyDataType");
		outStruct.setJaxbPackageName("com.legstar.test.coxb." + name);
		outStruct.setCicsContainer("ReplyData");
		op1.getOutput().add(outStruct);
		
		outStruct = new CixsStructure();
		outStruct.setJaxbType("ReplyStatusType");
		outStruct.setJaxbPackageName("com.legstar.test.coxb." + name);
		outStruct.setCicsContainer("ReplyStatus");
		op1.getOutput().add(outStruct);
		
		sv.getCixsOperations().add(op1);
		
		return sv;
	}
	
	private void printFile(File f) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(f));
        String str = in.readLine();
        while (str != null) {
    		System.out.println(str);
        	str = in.readLine();
        }
        in.close();
	}
}
