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
package com.legstar.schemagen;

/**
 * This class is used to generate a JNI wrapper for the COB2XSD C module.
 * COB2XSD transforms a Cobol source into an annotated XML schema.
 *
 * @author Fady Moussallam
 * 
 */
public class COB2XSDJNIWrapper {
	
	/**
	 * Native interface to CB2XSD main entry point. 
	 * 
	 * @param inVars a Class encapsulating input parameters
	 * @param outVars a Class encapsulating output parameters
	 * @return the native call return code 
	 * */
	public final native int cob2xsd(final InVars inVars, final OutVars outVars);
	static 
	{
		System.loadLibrary("COB2XSDJNIWrapper"); 
	}
	
	/**
	 * Class encapsulating COB2XSD input parameters.
	 */
	public class InVars {
		/** True generate verbose traces on stdout. */
		public boolean debugMode;
		
		/** Cobol source. */
		public String inFile;
		
		/** Name of XSD root element. Null means ignore any upper level simple
		 *  types. */
		public String inRootName; 
		
		/** Cobol compiler options in effect. */
		public CobolOptions cobolOptions; 
		
		/** XML schema generation options. */
		public XsdOptions xsdOptions;
		
		/** Output XML schema file name. */
		public String outFile;           
	}
	
	/**
	 * Class encapsulating COB2XSD output parameters.
	 */
	public class OutVars {
		/** Message returned from COB2XSD. */
		public String message;
	}
	
	/** Cobol compiler related options options. */
	public class CobolOptions {
		/** true = DEBUG lines are to be parsed.      */
		public boolean includeDebugLines = false; 
		/** true = TRUNC(BIN) option is in effect.          */
		public boolean truncBin = false;  
		/** The currency symbol used in numerics.      */
		public String currencySign = "$"; 
		/** true = Comma is the decimal separator.   */
		public boolean decimalPointIsComma = false;
		/** true = NSYMBOL(DBCS) option is in effect.       */
		public boolean nsymbolDbcs = false; 
		/** true = QUOTE option in effect otherwise APOST.  */
		public boolean quote = false;          
	}
	
	/** XSD generation options. */
	public class XsdOptions {
		 /** True = Replace - sign from cobol names. */
		public boolean replaceMinus = false; 
		
		/** Char to use to replace - sign.           */
		public String replaceMinusChar = "_"; 
		
		/** True = Remove - sign from cobol names.  */
		public boolean removeMinus = true; 
		
		/** True = Comma is the decimal separator.  */
		public boolean uppercaseToLower = true;
		
		/** True = Uppercase first char and chars following minus sign.  */
		public boolean firstcharUpper = true; 
		
		/** Used to build complex type names.       */
		public String typeSuffix = "Type";

		/** Xml schema namespace prefix.              */
		public String xsPrefix = "xs"; 
		
		/** XML Schema namespace. */
		public String xsNs = "http://www.w3.org/2001/XMLSchema"; 
		
		/** Target namespace prefix.                 */
		public String xsnsPrefix = "xsns"; 
		
		/** Target namespace.           */
		public String xsnsNs = "http://tempuri/schemagen";
		
		/** Xml schema Cobol appinfo prefix.          */
		public String xscbPrefix = "cb"; 
		
		/** The Cobol appinfo schema namespace.   */
		public String xscbNs = "http://www.legsem.com/xml/ns/coxb"; 
		
		/** Jaxb namespace prefix .                   */
		public String xsjaxbPrefix = "jaxb"; 
		
		/** Jaxb namespace.     */
		public String xsjaxbNs = "http://java.sun.com/xml/ns/jaxb";
		
		/** Jaxb version.                             */
		public String xsjaxbVersion = "2.0"; 
		
		/** Jaxb package used for generated classes.     */
		public String xsjaxbPackage = "com.tempuri"; 
		
		/** Add schema definitions.               */
		public boolean xsdHeaderFooter = true;   
	} 
}	
