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
import java.util.Hashtable;

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
	
	/** Program CICS channel XML element. */
	private static final String CICS_CHANNEL_E = "cics-channel";
	
	/** A CICS container XML element. */
	private static final String CICS_CONTAINER_E = "cics-container";
	
	/** Operation input XML element. */
	private static final String INPUT_E = "input";
	
	/** Operation output XML element. */
	private static final String OUTPUT_E = "output";
	
	/** Input or output JAXB type XML attribute. */
	private static final String JAXB_TYPE_E = "jaxb-type";
	
	/** Input or output JAXB package XML attribute. */
	private static final String JAXB_PKG_E = "jaxb-package";
	
	/** Input or output jaxb property name XML attribute. */
	private static final String JAXB_PROP_NAME_E = "jaxb-property-name";
	
	/** Input or output jaxb field name XML attribute. */
	private static final String JAXB_FIELD_NAME_E = "jaxb-field-name";
	
	/** Input or output binding property name XML attribute. */
	private static final String BIND_PROP_NAME_E = "bind-property-name";
	
	/** Input or output binding field name XML attribute. */
	private static final String BIND_FIELD_NAME_E = "bind-field-name";
	
	/** Input or output JAXB classes location XML attribute. */
	private static final String JAXB_LOC_E = "jaxb-classes-location";
	
	/** Input or output strategy to resolve choice alternatives. */
	private static final String CHOICE_STRATEGY_E = "choice-strategy";
	
	/** Table used to guarantee unicity for bind variable names. */
	private Hashtable < String, String > bindVarNamesMap =
		new Hashtable < String, String >();


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
	        if (operation.getChannel() != null
	        		&& operation.getChannel().length() > 0) {
		        out.write("<" + CICS_CHANNEL_E + ">"
		        		+  operation.getChannel()
	                    + "</" + CICS_CHANNEL_E + ">");
	        }
	        for (CixsStructure structure : operation.getInputStructures()) {
	        	writeStructureContent(INPUT_E, structure, out);
	        }
	        for (CixsStructure structure : operation.getOutputStructures()) {
	        	writeStructureContent(OUTPUT_E, structure, out);
	        }
	        
	        out.write("</" + OPERATION_E + ">");
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
		}
		
	}

	/**
	 * Write the elements describing a structure in already opened file.
	 * 
	 * @param elementName the input or output element name
	 * @param structure the structure data
	 * @param out an opened buffered writer
	 * @throws CixsException if an IOException occurs
	 */
	public final void writeStructureContent(
			final String elementName,
			final CixsStructure structure,
			final BufferedWriter out) throws CixsException {
		
		try {
		    out.write("<" + elementName); 
		    out.write(' ' + JAXB_TYPE_E + "=\""
		    		+ structure.getJaxbType() + '\"');
		    out.write(' ' + JAXB_PKG_E + "=\""
		    		+ structure.getJaxbPackageName() + '\"');
		    out.write(' ' + JAXB_LOC_E + "=\""
		    		+ structure.getJaxbPackageName().
		    		replace('.', '/') + '\"');
		    String jaxbPropertyName = getJaxbPropertyName(
    				structure.getJaxbType());
		    out.write(' ' + JAXB_PROP_NAME_E + "=\""
		    		+ jaxbPropertyName + '\"');
		    out.write(' ' + JAXB_FIELD_NAME_E + "=\""
		    		+ fieldNameFromPropertyName(jaxbPropertyName) + '\"');
		    String bindFieldName = getBindFieldName(elementName, 
    				structure.getJaxbType());
		    out.write(' ' + BIND_PROP_NAME_E + "=\""
		    		+ propertyNameFromFieldName(bindFieldName) + '\"');
		    out.write(' ' + BIND_FIELD_NAME_E + "=\""
		    		+ bindFieldName + '\"');
			if ((structure.getChoiceStrategy() != null)
					&& (structure.getChoiceStrategy().length() > 0)) {
		        out.write(' ' + CHOICE_STRATEGY_E + "=\""
		        		+ structure.getChoiceStrategy() + '\"');
			}
			if ((structure.getContainer() != null)
					&& (structure.getContainer().length() > 0)) {
		        out.write(' ' + CICS_CONTAINER_E + "=\""
		        		+ structure.getContainer() + '\"');
			}
		    out.write("/>"); 
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CixsException("IOException " + e.getMessage()));
		}
	}
	
	/**
	 * Creates a Java field name for a jaxb type instance. The
	 * name is obtained by stripping any trailing Type suffix.
	 * @param jaxbType the associated jaxb type
	 * @return a unique variable name
	 */
	private String getJaxbPropertyName(
			final String jaxbType) {

		String varName = jaxbType;
		if (varName.endsWith("Type")) {
			varName = varName.substring(0, varName.length() - 4);
		}
		return varName;
	}

	/**
	 * Creates a unique Java field name for a binding variable. The
	 * name is obtained by concatenating the parent element name
	 * (either input or output) with the jaxb type. The result is
	 * then looked up a table and a suffix is appended to get a 
	 * unique name.
	 * @param parentElementName either input or output
	 * @param jaxbType the associated jaxb type
	 * @return a unique variable name
	 */
	private String getBindFieldName(
			final String parentElementName,
			final String jaxbType) {
		String varName = parentElementName + getJaxbPropertyName(jaxbType);
		if (bindVarNamesMap.containsKey(varName)) {
			int suffix = 0;
			String suffixedName = varName + suffix;
			while (bindVarNamesMap.containsKey(suffixedName)) {
				suffix++;
				suffixedName = varName + suffix;
			}
			varName = suffixedName;
		}
		bindVarNamesMap.put(varName, jaxbType);
		return varName;
	}
	
	/**
	 * A property name normally starts with an uppercase character where
	 * field names start with lowercase.
	 * @param propertyName the property name from which to derive a field name
	 * @return the field name
	 */
	public static final String fieldNameFromPropertyName(
			final String propertyName) {
		if (propertyName == null) {
			return null;
		}
		if (propertyName.length() == 0) {
			return "";
		}
		return propertyName.substring(0, 1).toLowerCase()
			+ propertyName.substring(1, propertyName.length());
	}

	/**
	 * A field name normally starts with an lower character where
	 * property names start with uppercase.
	 * @param fieldName the field name from which to derive a property name
	 * @return the field name
	 */
	public static final String propertyNameFromFieldName(
			final String fieldName) {
		if (fieldName == null) {
			return null;
		}
		if (fieldName.length() == 0) {
			return "";
		}
		return fieldName.substring(0, 1).toUpperCase()
			+ fieldName.substring(1, fieldName.length());
	}
}
