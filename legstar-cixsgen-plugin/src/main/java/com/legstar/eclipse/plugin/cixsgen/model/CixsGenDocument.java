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
package com.legstar.eclipse.plugin.cixsgen.model;

import java.util.Properties;

/**
 * Legacy Web Service Model. A serializable properties file.
 */
public class CixsGenDocument extends Properties {
	/** The serial ID. */
	private static final long serialVersionUID = -9080715447034558682L;
	/** Web service name. */
	public static final String WS_NAME = "wsname";
	/** Operation name. */
	public static final String OP_NAME = "operation";
	/** Host program name. */
	public static final String OP_PROG = "program";
	/** Output location of jaxb classes. */
	public static final String OP_JAXB_OUTLOC = "outputlocation";
	/** Input package name. */
	public static final String IN_PKG = "inputpackage";
	/** Input type name. */
	public static final String IN_TYPE = "inputtype";
	/** Output package name. */
	public static final String OUT_PKG = "outputpackage";
	/** Output type name. */
	public static final String OUT_TYPE = "outputtype";
}
