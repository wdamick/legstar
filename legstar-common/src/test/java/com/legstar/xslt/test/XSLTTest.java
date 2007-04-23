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
package com.legstar.xslt.test;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import junit.framework.TestCase;

public class XSLTTest extends TestCase {
	
	/** Check XSLT 2.0 capabilities */
	public void testXSLT() throws IOException {
		
        /* Create a temporary output folder. */
		java.io.File temp = java.io.File.createTempFile("dummy", ".tmp");
        temp.deleteOnExit();
		String dest = temp.getAbsolutePath();
    
		com.legstar.xslt.XSLTLegstar xsltTask = new com.legstar.xslt.XSLTLegstar();
		xsltTask.setXmlfile("src/test/resources/xml/xmltest-01.xml");
		xsltTask.setXsltfile("/xslt/xsltest-01.xsl");
		xsltTask.setResultfile(dest);
		xsltTask.execute();
		
		/* The generated class name is different from the dummy temp file. It
		 * is under a package hierarchy. */
		
        BufferedReader in = new BufferedReader(new FileReader(temp));
        String result = "";
        String str = in.readLine();
        while (str != null && str.length() > 0) {
        	result += str;
        	str = in.readLine();
        }
        in.close();
		assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?><COUNTRY name=\"UK\">   <YEAR year=\"1988\">      <TITLE>Hide your heart</TITLE>   </YEAR>   <YEAR year=\"1990\">      <TITLE>Still got the blues</TITLE>      <TITLE>This is US</TITLE>   </YEAR></COUNTRY><COUNTRY name=\"USA\">   <YEAR year=\"1985\">      <TITLE>Empire Burlesque</TITLE>   </YEAR></COUNTRY>", result);
		
	}

}
