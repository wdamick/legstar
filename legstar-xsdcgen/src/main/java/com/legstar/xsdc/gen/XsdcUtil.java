/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
