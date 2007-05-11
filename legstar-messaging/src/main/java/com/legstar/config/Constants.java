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
package com.legstar.config;

/**
 * Configuration-related constant values.
 */
public final class Constants {
	
	/** Configuration XPath location for an endpoint. */
	public static final String HOST_ENDPOINT_KEY =
		"hostEndPoints/hostEndPoint";
	
	/** Configuration XPath location for the connection factory. */
	public static final String HOST_CONNECTION_FACTORY_KEY =
		"hostConnectionfactoryClass";
	
	/** Configuration XPath location endpoint. */
	public static final String ENDPOINT_NAME_KEY = "hostEndPoint";
	
	/** Configuration XPath location endpoint name attribute. */
	public static final String ENDPOINT_NAME_ATTR_KEY = "@name";
	
	
	/** Property identifier for the host program name. */
	public static final String CICS_PROGRAM_KEY = "CICSProgram";
	
	/** Property identifier for program commarea length. */
	public static final String CICS_LENGTH_KEY = "CICSLength";
	
	/** Property identifier for program input data length. */
	public static final String CICS_DATALEN_KEY = "CICSDataLength";
	
	/** Property identifier for program system ID. */
	public static final String CICS_SYSID_KEY = "CICSSysID";
	
	/** Property identifier for program sync on return parm. */
	public static final String CICS_SYNCONRET_KEY = "CICSSyncOnReturn";
	
	/** Property identifier for program transaction ID. */
	public static final String CICS_TRANSID_KEY = "CICSTransID";
	
	/** Property identifier for channel ID. */
	public static final String CICS_CHANNEL_KEY = "CICSChannel";
	
	/** Property identifier for output containers array. */
	public static final String CICS_OUT_CONTAINERS_KEY = "CICSOutContainers";
	
	/** Property identifier for host character set. */
	public static final String HOST_CHARSET_KEY = "hostCharset";
	
	/** Property identifier for host user ID. */
	public static final String HOST_USERID_KEY = "hostUserID";
	
	/** Property identifier for host password. */
	public static final String HOST_PASSWORD_KEY = "hostPassword";

	/** Property identifier for host trace mode. */
	public static final String HOST_TRACEMODE_KEY = "hostTraceMode";

}
