package com.legstar.c2ws;


import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import com.legstar.c2ws.C2wsConfigurationManager;
import com.legstar.c2ws.C2wsWSDescriptor;

import junit.framework.TestCase;

public class C2wsConfigurationManagerTest extends TestCase {
	
	private static final String TARGET_DIR = "target/test-classes";
	
	public void testLoadNonexistantFile() throws Exception {
		try {
			C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("c2wsconfigzut.xml");
			fail(c2wsConfigManager.getC2wsConfigFileName());
		} catch( C2wsConfigurationException e) {
			assertEquals("Unable to locate resource c2wsconfigzut.xml", e.getMessage());
		}
	}
	
	public void testGetService() throws Exception {
		C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("legstar-c2wsrt-config.xml");
		C2wsWSDescriptor wsd = c2wsConfigManager.getWebServiceDescriptor("CultureInfo");
		assertEquals("http://sample.c2ws.legstar.com/", wsd.getWsdlTargetNamespace());
		assertEquals("CultureInfoImplService", wsd.getWsdlName());
		assertEquals("CultureInfoImplPort", wsd.getWsdlPort());
		assertEquals("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl", wsd.getWsdlUrl());
		assertEquals("com.legstar.test.coxb.cultureinfo", wsd.getJaxbRequest().getPackageName());
		assertEquals("GetInfoType", wsd.getJaxbRequest().getTypeName());
		assertEquals("GetInfo", wsd.getJaxbRequest().getElementName());
		assertEquals("com.legstar.test.coxb.cultureinfo", wsd.getJaxbResponse().getPackageName());
		assertEquals("GetInfoResponseType", wsd.getJaxbResponse().getTypeName());
		assertEquals("GetInfoResponse", wsd.getJaxbResponse().getElementName());
	}
	
	public void testGetServiceFromCache() throws Exception {
		C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("legstar-c2wsrt-config.xml");
		C2wsWSDescriptor wsd = c2wsConfigManager.getWebServiceDescriptor("CultureInfo");
		assertEquals("http://sample.c2ws.legstar.com/", wsd.getWsdlTargetNamespace());
		assertEquals("CultureInfoImplService", wsd.getWsdlName());
		assertEquals("CultureInfoImplPort", wsd.getWsdlPort());
		assertEquals("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl", wsd.getWsdlUrl());
		assertEquals("com.legstar.test.coxb.cultureinfo", wsd.getJaxbRequest().getPackageName());
		assertEquals("GetInfoType", wsd.getJaxbRequest().getTypeName());
		assertEquals("GetInfo", wsd.getJaxbRequest().getElementName());
		assertEquals("com.legstar.test.coxb.cultureinfo", wsd.getJaxbResponse().getPackageName());
		assertEquals("GetInfoResponseType", wsd.getJaxbResponse().getTypeName());
		assertEquals("GetInfoResponse", wsd.getJaxbResponse().getElementName());
		wsd = c2wsConfigManager.getWebServiceDescriptor("CultureInfo");
		assertEquals("http://sample.c2ws.legstar.com/", wsd.getWsdlTargetNamespace());
	}

	public void testEmptyConfig() throws Exception {
		ensureNoFile("combined.xml");
		createCombinedConfigFile(0);
		C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("combined.xml");
		try {
			c2wsConfigManager.getWebServiceDescriptor("inner0");
		} catch( C2wsConfigurationException e) {
			assertEquals("Service inner0 has no descriptors in combined.xml", e.getMessage());
		}
	}
	
	public void testReloadStrategy() throws Exception {
		ensureNoFile("combined.xml");
		ensureNoFile("inner0.xml");
		ensureNoFile("inner1.xml");

		/* create a combined configuration file with a single inner config */
		createInnerFile("inner0");
		createCombinedConfigFile(1);
		
		/* chack that we have inner0 but no inner1 */
		C2wsConfigurationManager c2wsConfigManager = new C2wsConfigurationManager("combined.xml");
		C2wsWSDescriptor wsd = c2wsConfigManager.getWebServiceDescriptor("inner0");
		assertEquals("http://sample.c2ws.legstar.com/", wsd.getWsdlTargetNamespace());
		try {
			c2wsConfigManager.getWebServiceDescriptor("inner1");
		} catch( C2wsConfigurationException e) {
			assertEquals("Service inner1 has no descriptors in combined.xml", e.getMessage());
		}
		
		/* FileChangedReloadingStrategy will not check for modifications
		 * immediatly, there is a 5 seconds delay */
		Thread.sleep(5500);

		/* create an additional inner config and change the combined configuration file */
		createInnerFile("inner1");
		createCombinedConfigFile(2);
		
		
		C2wsWSDescriptor wsd1 = c2wsConfigManager.getWebServiceDescriptor("inner1");
		assertEquals("http://sample.c2ws.legstar.com/", wsd1.getWsdlTargetNamespace());
		ensureNoFile("combined.xml");
		ensureNoFile("inner0.xml");
		ensureNoFile("inner1.xml");
	}
		
	private void ensureNoFile(String fileName) {
        File file = new File(TARGET_DIR + '/' + fileName);
        if (file.exists()) {
            file.delete();
        }
        assertFalse("The file should not exist", file.exists());
	}
	
	private void createCombinedConfigFile(int inners) throws IOException {
		File path = new File(TARGET_DIR);
		File confFile = new File(path, "combined.xml");
		PrintWriter out = null;

		try	{
			if (!path.exists()) {
			    assertTrue(path.mkdir());
			}
		    out = new PrintWriter(new FileWriter(confFile));
		    out.println("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>");
		    out.println("<configuration>");
		    out.println("<additional>");
		    for (int i = 0; i < inners; i++) {
			    out.println("<xml fileName=\"inner" + i + ".xml\" config-name=\"inner" + i + "\"/>");
		    }
		    out.println("</additional>");
		    out.println("</configuration>");
		    out.close();
		    out = null;
	    }
	    finally  {
	        if (out != null) {
	            out.close();
	        }
	    }
	}

	private void createInnerFile(String fileName) throws IOException {
		File path = new File(TARGET_DIR);
		File confFile = new File(path, fileName + ".xml");
		PrintWriter out = null;

		try	{
			if (!path.exists()) {
			    assertTrue(path.mkdir());
			}
		    out = new PrintWriter(new FileWriter(confFile));
		    out.println("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>");
		    out.println("<service>");
		    out.println("<wsdl>");
		    out.println("<targetNamespace>http://sample.c2ws.legstar.com/</targetNamespace>");
		    out.println("</wsdl>");
		    out.println("</service>");
		    out.close();
		    out = null;
	    }
	    finally  {
	        if (out != null) {
	            out.close();
	        }
	    }
	}

}
