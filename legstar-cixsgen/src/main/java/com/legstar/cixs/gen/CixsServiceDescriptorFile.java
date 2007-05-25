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
package com.legstar.cixs.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.BufferedWriter;
import java.io.FileWriter;

/**
 * This class contains the logic to create service descriptors which are XML
 * files that describe a service and can be used by XSL transform to generate
 * service endpoint code.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsServiceDescriptorFile {
	
	/** Current service descriptor file. */
	private File mServiceDescriptorFile;
	
	/** Pattern for temporary files. */
	private static final String TEMP_PATTERN = "legstar";
	
	/** Suffix for temporary files. */
	private static final String TEMP_SUFFIX = ".tmp";
	

	/**
	 * Create a temporary file to hold service descriptors.
	 * 
	 * @throws CixsException if temporary file cannot be created
	 */
	public CixsServiceDescriptorFile() throws CixsException {
		try {
	        /* Create a temporary file. */
			mServiceDescriptorFile = File.createTempFile(TEMP_PATTERN,
					TEMP_SUFFIX);
	    
	        /* Delete temporary file when program exits.*/
			mServiceDescriptorFile.deleteOnExit();
	    
	    } catch (IOException e) {
			throw (new CixsException(e));
	    }
	}
	
	/**
	 * Create a temporary file to hold service descriptors and serialize the
	 * service in it.
	 * 
	 * @param service the service data
	 * @throws CixsException if temporary file cannot be created
	 */
	public CixsServiceDescriptorFile(
			final CixsService service) throws CixsException {
		this();
		writeServiceDescriptorFile(service);
	}
	
	/**
	 * Populate an XML file with a service description.
	 * 
	 * @param service the service data
	 * @throws CixsException if an IOException occurs
	 */
	public final void writeServiceDescriptorFile(
			final CixsService service) throws CixsException {
		
        /* Write XML description to temp file */
        BufferedWriter out;
		try {
			out = new BufferedWriter(new FileWriter(mServiceDescriptorFile));
			out.write(service.serialize());
	        out.close();
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
		}
		
	}
	
	/**
	 * Returns the content of the service descriptor file as a String.
	 * @return content of service descriptor file
	 * @throws CixsException if content cannot be read
	 */
	public final String getContentAsString() throws CixsException {
	    StringBuffer sb = new StringBuffer();
		try {
	        BufferedReader in = new BufferedReader(
	        		new FileReader(mServiceDescriptorFile));
	        String str = in.readLine();
	        while (str != null && str.length() > 0) {
	        	sb.append(str);
	        	str = in.readLine();
	        }
	        in.close();
	        return sb.toString();
	    } catch (IOException e) {
			throw new CixsException(e);
	    }
	}
	
	/**
	 * @return the  Service Descriptor File
	 */
	public final File getServiceDescriptorFile() {
		return mServiceDescriptorFile;
	}

	/**
	 * @param serviceDescriptorFile the Service Descriptor File to set
	 */
	public final void setServiceDescriptorFile(
			final File serviceDescriptorFile) {
		mServiceDescriptorFile = serviceDescriptorFile;
	}
	
}
