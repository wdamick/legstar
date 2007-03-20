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
package com.legstar.cixs.http.test;

import junit.framework.TestCase;

import com.legstar.cixs.coxb.CIXSProgram;
import com.legstar.cixs.http.*;

/**
 * @author Fady Moussallam
 *
 * The Class is a superclass of the actual test cases
 * 
 */
public abstract class CICWAbstractTest extends TestCase {

	protected CICWConnectionFactory m_CF;
	protected CICWConnection m_conn;
	
	private static final boolean DEBUG_MODE = false;

	/**
	 * Sets up the test fixture.
	 *
	 * Called before every test case method.
	 */
	protected void setUp() throws Exception {
		super.setUp();
		/* create a CICW connection to the host based on properties file*/
		m_CF = new CICWConnectionFactory();
     	CICWHost host = new CICWHost("hostconnection.properties");
		m_conn = m_CF.createConnection(host);
	}
	/**
	 * Tears down the test fixture.
	 *
	 * Called after every test case method.
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	

    /**
     * Run a test case
     * 
     * @param testDescription user friendly test label
     * @param expectedOutcome should the test return false or true
     */
    public byte[] testProgram(CIXSProgram program, String strRequest, String testDescription, boolean expectedOutcome) {
        if (DEBUG_MODE) {
        	System.out.println(testDescription +" test started");
        }
        byte[] response = null;
        
        /** Display requested data */
    	byte[] request = string2Byte(strRequest);
        if (DEBUG_MODE) {
	        System.out.println("     Request data:");
	    	CEbcdic.printHex(request);
        }

    	/** Execute the request */
    	try {
    		response = m_conn.invoke(program, request);
    		if (!expectedOutcome)
                fail(testDescription + " test failed");
    	}
    	catch (Exception e) {
    		if (expectedOutcome) {
    			e.printStackTrace();
    			fail(testDescription + " test failed");
    		}
    		else {
    	        if (DEBUG_MODE) {
	    			System.out.println(e.getMessage());
    	        }
    		}
    	}
        
        /** Display response (even if test failed since we might have an error message
         * in the response) */
        if (DEBUG_MODE) {
	        System.out.println("     Response data:");
	    	CEbcdic.printHex(response);
	        System.out.println(testDescription + " test ended");
	        System.out.println("");
       }

        return response;
    }
   
    /**
     * Turns a string containing hex encoding into a byte array
     * @param strRequest
     * @return
     */
    protected byte[] string2Byte(String strRequest) {
		byte[] request = null;
		
    	if (null != strRequest) {
    		request = new byte[strRequest.length() / 2];
			for( int i = 0; i < strRequest.length(); i+=2 ) 
			{ 
			    String digits = strRequest.substring( i, i+2 ); 
			    request[ i / 2 ] = (byte)Integer.parseInt( digits, 16 ); 
			} 
		}
    	return request;
    }
	/**
	 * Helper method to dump field content in hex
	 * 
	 * @return a string with hexadecimal representation of the field content
	 */
	public String toHexString(byte[] hostBytes) {
		
		if (hostBytes == null)
			return null;
		
		StringBuffer hexString = new StringBuffer("");
		for (int i = 0; i < hostBytes.length; i++) {
			hexString.append(Integer.toHexString(hostBytes[i] & 0xFF | 0x100).substring(1,3));
		}
		
		return hexString.toString();
	}

	/**
	 * Helper method to populate a byte array from a hex string representation
	 * 
	 * @return an initialized byte array
	 */
	public byte[] toByteArray( String string) {
		if (string == null)
			return null;
		byte[] hostBytes = new byte[string.length()/2];
		for (int i = 0; i < string.length(); i+=2)
			hostBytes[i / 2] = (byte)Integer.parseInt( string.substring(i,i+2), 16 );
		return hostBytes;
	}

}
