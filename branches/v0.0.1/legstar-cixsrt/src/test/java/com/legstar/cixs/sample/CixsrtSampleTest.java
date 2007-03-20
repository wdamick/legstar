/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.cixs.sample;

import java.io.IOException;

import com.legstar.cixs.coxb.CIXSException;
import com.legstar.cixs.coxb.CIXSHeader;
import com.legstar.cixs.coxb.CIXSInvokerFactory;
import com.legstar.cixs.coxb.CIXSParameter;
import com.legstar.cixs.coxb.CIXSProgram;
import com.legstar.cixs.coxb.ICIXSInvoker;
import com.legstar.cixs.http.CICWConnection;
import com.legstar.cixs.http.CICWConnectionFactory;
import com.legstar.cixs.http.CICWException;
import com.legstar.cixs.http.CICWHost;

import com.legstar.test.coxb.lsfileae.ObjectFactory;
import com.legstar.test.coxb.lsfileae.DfhcommareaType;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding;

import junit.framework.TestCase;

/**
 * Sample usage of the Cixsrt library.
 */
public class CixsrtSampleTest extends TestCase {
	
	/** Use low-level HTTP call to a CICS program, sending data
	 * in zos format. In this sample, you are in charge of dealing
	 * with zos data.
	 * The properties files must be available from the classpath.
	 */ 
	public void testHttp() throws IOException, CICWException {

		/* A host connection is described by a properties file. */
		CICWConnectionFactory cf = new CICWConnectionFactory();
     	CICWHost host = new CICWHost("hostconnection.properties");
     	CICWConnection cics = cf.createConnection(host);
		
        /* A CICS program is described by a properties file */
		CIXSProgram program = new CIXSProgram("lsfileae.properties");

        /* Request and Response are byte arrays of zos data */
		byte[] response = null;
    	byte[] request = toByteArray("f0f0f0f1f0f0");

    	/* Invoke the remote CICS program. Might raise a CICWException.*/
    	response = cics.invoke(program, request);
    	
    	assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c", toHexString(response));

	}
	
	/** Use COXB binding classes to create zos data from java prior
	 * to using the low-level HTTP connectivity to zos.
	 * The properties files must be available from the classpath.
	 * @throws CIXSException 
	 */ 
	public void testCoxb() throws CIXSException {

		/* Create a JAXB object for the request data */
		ObjectFactory jaxbOf = new ObjectFactory();
		DfhcommareaType jaxbIn = jaxbOf.createDfhcommareaType();
		jaxbIn.setComNumber(100);
		
		/* Create a COXB request object bound to a JAXB object */
		DfhcommareaTypeBinding coxbIn =
			new DfhcommareaTypeBinding(jaxbOf, jaxbIn);
		
		/* Create a COXB reply object */
		DfhcommareaTypeBinding CoxbOut =
			new DfhcommareaTypeBinding(jaxbOf);
		
        /* Create an invoker and bind it to imput and output Coxb objects */
		CIXSInvokerFactory cf = new CIXSInvokerFactory();
		ICIXSInvoker invoker = cf.createInvoker();
		CIXSParameter inParameter = invoker.createParameter();
		inParameter.setComplexBinding(coxbIn);
		CIXSParameter outParameter = invoker.createParameter();
		outParameter.setComplexBinding(CoxbOut);

		/* Call remote CICS program */
		invoker.initialize(new CIXSHeader(), "lsfileae");
		invoker.invoke(inParameter, outParameter);
		
		/* Get the response JAXB object */
		DfhcommareaType jaxbOut = CoxbOut.getJaxbObject();
		
    	/* Print result*/
    	assertEquals("S. D. BORMAN        ",  jaxbOut.getComPersonal().getComName());

	}
	/**
	 * Helper method to populate a byte array from a hex string representation.
	 * @return an initialized byte array
	 */
	private byte[] toByteArray(String str) {

		if (str == null) {
			return null;
		}

		byte[] hostBytes = new byte[str.length() / 2];
		for (int i = 0; i < str.length(); i += 2) {
			hostBytes[i / 2] = (byte) Integer.parseInt(
					str.substring(i, i + 2), 16 );
		}
		return hostBytes;
	}
	
	/**
	 * Helper method to dump field content in hex.
	 * @return a string with hexadecimal representation of the field content
	 */
	public String toHexString(byte[] hostBytes) {
		
		if (hostBytes == null) {
			return null;
		}
		
		StringBuffer hexString = new StringBuffer("");
		for (int i = 0; i < hostBytes.length; i++) {
			hexString.append(Integer.toHexString(
					hostBytes[i] & 0xFF | 0x100).substring(1,3));
		}
		
		return hexString.toString();
	}

}
