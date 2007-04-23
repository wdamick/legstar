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
package com.legstar.coxb.misc.test;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import junit.framework.TestCase;

public class XSLTTest extends TestCase {
	
	public void testXSLT() throws IOException {
		
        /* Create a temporary output folder. */
		java.io.File temp = java.io.File.createTempFile("dummy", ".tmp");
        temp.deleteOnExit();
		String dest = temp.getAbsolutePath();
    
		com.legstar.xslt.XSLTLegstar xsltTask = new com.legstar.xslt.XSLTLegstar();
		xsltTask.setXmlfile("src/xml-models/coxb-td-lsfileae-dfhcommarea.xml");
		xsltTask.setXsltfile("/xslt/coxb-bind.xsl");
		xsltTask.setResultfile(dest);
		xsltTask.execute();
		
		/* The generated class name is different from the dummy temp file. It
		 * is under a package hierarchy. */
		
		String resFile = temp.getParentFile().getAbsolutePath();
		resFile += "/com/legstar/test/coxb/lsfileae/bind/DfhcommareaTypeBinding.java";
        BufferedReader in = new BufferedReader(new FileReader(resFile));
        String str = in.readLine();
        String str2 = in.readLine();
        in.close();
		assertEquals("", str);
		assertEquals("package com.legstar.test.coxb.lsfileae.bind;", str2);
		
	}

}
