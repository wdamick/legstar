package com.legstar.xsdc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import junit.framework.TestCase;

public class PackageNameTest extends TestCase {
	
	public void testMixedCaseName() throws Exception {
    	XsdCobolAnnotator xca = new XsdCobolAnnotator();
    	xca.setInputXsdUri(new File(
    			"src/test/resources/singleSimpleElement.xsd").toURI());
    	xca.setTargetDir(new File("target"));
		xca.execute();
		/* Read the resulting output source*/
		String result = getSource("target/singleSimpleElement.xsd");
		assertTrue(result.contains("<jaxb:package name=\"com.example.finance.creditcardfaults.xsd\"/>"));
	}
	
	private String getSource(final String fileName) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(fileName));
        StringBuffer res = new StringBuffer();
        String str = in.readLine();
        while (str != null) {
        	res.append(str);
        	str = in.readLine();
        }
        in.close();
		return res.toString();
	}

}
