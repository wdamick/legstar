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
package com.legstar.xsdc.gen;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Useful utility methods.
 */
public final class XsdcUtil {
	
	/** Utility class not meant for instanciation. */
	private XsdcUtil() {
		
	}
	/**
	 * Loads a properties file from classpath into memory.
	 * @param loadedClass class requesting resource to load
	 * @param propFileName the properties file name
	 * @return the in-memory properties file
	 * @throws IOException if file cannot be loaded
	 */
	public static Properties loadFromPropFile(
			final Class < ? > loadedClass,
			final String propFileName) throws IOException {
		
		/* load the program properties files */
		Properties properties = new Properties();
		
		InputStream in =
			loadedClass.getClassLoader().
			getResourceAsStream(propFileName);
		if (in == null) {
			throw (new FileNotFoundException(propFileName));
		}
		properties.load(in);
		in.close();
		return properties;
	}
	
	/**
	 * Get a String value from a properties file.
	 * @param properties an in-memory property file
	 * @param key the key whose value is to be returned
	 * @return the value as a string
	 */
	public static String getStringOption(
			final Properties properties,
			final String key) {
		return (String) properties.get(key);
	}

	/**
	 * Get an int value from a properties file.
	 * @param properties an in-memory property file
	 * @param key the key whose value is to be returned
	 * @return the value as an int
	 */
	public static int getIntOption(
			final Properties properties,
			final String key) {
		return new Integer((String) properties.get(key));
	}

}
