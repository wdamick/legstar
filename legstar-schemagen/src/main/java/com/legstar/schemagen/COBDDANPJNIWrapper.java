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
 * This class is used to generate a JNI wrapper for the COBDDANP C module.
 * COBDDANP analyzes a Cobol picture clause and parses its elements into
 * individual properties readily available to java code. 
 *
 * @author Fady Moussallam
 * 
 */
public class COBDDANPJNIWrapper {

	/**
	 * Native interface to COBDDANP main entry point. 
	 * 
	 * @param inVars a Class encapsulating input parameters
	 * @param outVars a Class encapsulating output parameters
	 * @return the native call return code 
	 * */
	public final native int anapict(final InVars inVars, final OutVars outVars);
	  static 
	  {
	    System.loadLibrary("COBDDANPJNIWrapper"); 
	  }
	  
	/**
	 * Class encapsulating COBDDANP input parameters.
	 */
	public class InVars {
		/** True generate verbose traces on stdout. */
		public boolean debugMode;

		/** The Cobol usage clause. */
		public String usage;
		
		/** The Cobol picture clause. */
		public String picture;
		
		/** The Cobol sign is separate clause. */
		public boolean signSeparate;
	}

	/**
	 * Class encapsulating COBDDANP output parameters.
	 */
	public class OutVars {
		
		/** The cobol data element type. */
		public int dataType;
		
		/** The cobol total number of digits for numerics. */
		public int totalDigits;
		
		/** The cobol fractional number of digits for numerics. */
		public int fractionDigits;
		
		/** Whether the cobol numeric is signed. */
		public int sign;
		
		/** The size of the cobol element in bytes. */
		public int byteLength;
		
		/** The native method return message. */
		public String message;
	}
}
