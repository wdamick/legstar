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
package com.legstar.eclipse.plugin.common;

/**
 * Various constants used in the legstar plugins.
 */
public class LegstarConstants {

	/** The BuildLogger class to use for AntRunner. */
	public static final String ANT_DEFAULT_LOGGER_CLASS =
		"org.apache.tools.ant.DefaultLogger"; 
	
	/** An alternative BuildLogger output messages on the console. */
	public static final String ANT_CONSOLE_LOGGER_CLASS =
		"com.legstar.eclipse.ant.AntConsoleLogger"; 
	
	/** The preference store ID for the selected Ant logger. */
	public static final String ANT_LOGGER_PREF = "AntLogger";
	
	/** The preference store ID for the selected Ant message level. */
	public static final String ANT_MSGLVL_PREF = "AntMessageLevel";
	
	/** The default Ant message level. */
	public static final int ANT_DEFAULT_MSGLVL = 2; 

}
