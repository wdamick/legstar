package com.legstar.cixs.http.test;

import java.io.IOException;

import com.legstar.cixs.coxb.CIXSProgram;
import com.legstar.cixs.http.CICWConnection;
import com.legstar.cixs.http.CICWConnectionFactory;
import com.legstar.cixs.http.CICWHost;

import junit.framework.TestCase;

public class VolumeTest extends TestCase {

	private static final int MAX_ITERATIONS = 1000;
	
	public void testLSFILEAE() throws IOException {
		
		long startTime = System.currentTimeMillis();
		for (int i = 0; i < MAX_ITERATIONS; i++) {
			/* create a CICW connection to the host based on properties file*/
			CICWConnectionFactory cf = new CICWConnectionFactory();
	     	CICWHost host = new CICWHost("hostconnection.properties");
	     	CICWConnection conn = cf.createConnection(host);
	     	CIXSProgram program = new CIXSProgram("lsfileae.properties");
	        byte[] response = null;
	    	byte[] request = string2Byte("f0f0f0f1f0f0");
	    	/** Execute the request */
	    	try {
	    		response = conn.invoke(program, request);
	        	assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c", toHexString(response));
	    	}
	    	catch (Exception e) {
				fail("testLSFILEAE failed" + e);
	    	}
		}
		long endTime = System.currentTimeMillis();
		System.out.println("Duration millisecs=" + (endTime - startTime));
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


}
