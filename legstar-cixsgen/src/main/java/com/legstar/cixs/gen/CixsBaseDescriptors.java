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

import java.io.File;
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
public class CixsBaseDescriptors {
	
	/** Pattern for temporary files. */
	private static final String TEMP_PATTERN = "legstar";
	
	/** Suffix for temporary files. */
	private static final String TEMP_SUFFIX = ".tmp";
	
	/** Service XML element. */
	private static final String SERVICE_E = "cixs-service";
	
	/** Service name XML element. */
	private static final String SERVICE_NAME_E = "service-name";
	
	/** Service package name XML element. */
	private static final String SERVICE_PKG_E = "service-endpoint-package";
	
	/** Service target namespace XML element. */
	private static final String SERVICE_NS_E = "service-targetnamespace";
	
	/** Operation XML element. */
	private static final String OPERATION_E = "cixs-operation";
	
	/** Operation name XML element. */
	private static final String OPERATION_NAME_E = "operation-name";
	
	/** Operation program name XML element. */
	private static final String PROGRAM_NAME_E = "program-name";
	
	/** Operation input XML element. */
	private static final String INPUT_E = "input";
	
	/** Operation output XML element. */
	private static final String OUTPUT_E = "output";
	
	/** Input or output JAXB type XML attribute. */
	private static final String JAXB_TYPE_E = "jaxb-type";
	
	/** Input or output JAXB package XML attribute. */
	private static final String JAXB_PKG_E = "jaxb-package";
	
	/** Input or output JAXB classes location XML attribute. */
	private static final String JAXB_LOC_E = "jaxb-classes-location";
	
	/** Input or output strategy to resolve choice alternatives. */
	private static final String CHOICE_STRATEGY_E = "choice-strategy";


	/**
	 * Create a temporary file to hold XML content.
	 * 
	 * @return the temporary file
	 * @throws CixsException if temporary file cannot be created
	 */
	public final File getTempFile() throws CixsException {
		File temp;
		try {
	        /* Create a temporary file. */
	        temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
	    
	        /* Delete temporary file when program exits.*/
	        temp.deleteOnExit();
	    
	    } catch (IOException e) {
	    	e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
	    }

	    return temp;
	}
	
	/**
	 * Populate an XML file with a service description.
	 * 
	 * @param service the service data
	 * @param temp an XML file
	 * @throws CixsException if an IOException occurs
	 */
	public final void createServiceContent(
			final CixsService service,
			final File temp) throws CixsException {
		
        /* Write to temp file */
        BufferedWriter out;
		try {
			out = new BufferedWriter(new FileWriter(temp));
			writeServiceContent(service, out);
	        out.close();
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
		}
		
	}
	
	/**
	 * Populate an XML file with an operation description.
	 * 
	 * @param operation the operation data
	 * @param temp an XML file
	 * @throws CixsException if an IOException occurs
	 */
	public final void createOperationContent(
			final CixsOperation operation,
			final File temp) throws CixsException {
		
        /* Write to temp file */
        BufferedWriter out;
		try {
			out = new BufferedWriter(new FileWriter(temp));
			writeOperationContent(operation, out);
	        out.close();
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
		}
		
	}
	
	/**
	 * Write the elements describing a service in already opened file.
	 * 
	 * @param service the service data
	 * @param out an opened buffered writer
	 * @throws CixsException if an IOException occurs
	 */
	public final void writeServiceContent(
			final CixsService service,
			final BufferedWriter out) throws CixsException {
		
		try {
	        out.write("<" + SERVICE_E + ">");
	        out.write("<" + SERVICE_NAME_E + ">"
	        		+  service.getServiceName()
                    + "</" + SERVICE_NAME_E + ">");
	        out.write("<" + SERVICE_PKG_E + ">"
	        		+  service.getEndpointPackageName()
                    + "</" + SERVICE_PKG_E + ">");
	        out.write("<" + SERVICE_NS_E + ">"
	        		+  service.getTargetNamespace()
                    + "</" + SERVICE_NS_E + ">");
			if (service.getOperations() != null) {
				for (CixsOperation operation : service.getOperations()) {
					writeOperationContent(operation, out);
				}
			}
	        out.write("</" + SERVICE_E + ">");
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
		}
		
	}
	
	/**
	 * Write the elements describing an operation in already opened file.
	 * 
	 * @param operation the operation data
	 * @param out an opened buffered writer
	 * @throws CixsException if an IOException occurs
	 */
	public final void writeOperationContent(
			final CixsOperation operation,
			final BufferedWriter out) throws CixsException {
		
		try {
	        out.write("<" + OPERATION_E + ">");
	        out.write("<" + OPERATION_NAME_E + ">"
	        		+  operation.getOperationName()
                    + "</" + OPERATION_NAME_E + ">");
	        out.write("<" + PROGRAM_NAME_E + ">"
	        		+  operation.getProgramName()
                    + "</" + PROGRAM_NAME_E + ">");
	        out.write("<" + INPUT_E); 
	        out.write(' ' + JAXB_TYPE_E + "=\""
	        		+ operation.getInputJaxbType() + '\"');
	        out.write(' ' + JAXB_PKG_E + "=\""
	        		+ operation.getInputJaxbPackageName() + '\"');
	        out.write(' ' + JAXB_LOC_E + "=\""
	        		+ operation.getInputJaxbPackageName().
	        		replace('.', '/') + '\"');
        	if ((operation.getInputChoiceStrategy() != null)
        			&& (operation.getInputChoiceStrategy().length() > 0)) {
    	        out.write(' ' + CHOICE_STRATEGY_E + "=\""
    	        		+ operation.getInputChoiceStrategy() + '\"');
        	}
	        out.write("/>"); 
	        
	        out.write("<" + OUTPUT_E); 
	        out.write(' ' + JAXB_TYPE_E + "=\""
	        		+ operation.getOutputJaxbType() + '\"');
	        out.write(' ' + JAXB_PKG_E + "=\""
	        		+ operation.getOutputJaxbPackageName() + '\"');
	        out.write(' ' + JAXB_LOC_E + "=\""
	        		+ operation.getOutputJaxbPackageName().
	        		replace('.', '/') + '\"');
       	if ((operation.getOutputChoiceStrategy() != null)
        			&& (operation.getOutputChoiceStrategy().length() > 0)) {
    	        out.write(' ' + CHOICE_STRATEGY_E + "=\""
    	        		+ operation.getOutputChoiceStrategy() + '\"');
        	}
	        out.write("/>"); 
	        
	        out.write("</" + OPERATION_E + ">");
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
		}
		
	}

}
