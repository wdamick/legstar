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
package com.legstar.cixs.model.util;


/**
 * Utility methods used by model classes.
 */
public final class ModelUtil {

    /** Suffix used for JAXB type variable names. */
    public static final String JAXB_TYPE_SUFFIX = "Type";

    /**
     * Defeats instantiation. Utility class.
     */
    private ModelUtil() {
     }
    
    /**
     * Create a valid Java class name from a given noun.
     * 
     * @param noun
     *            the characters to turn into a java class name
     * @return the Java class name
     */
    public static String classNormalize(final String noun) {
        String className = null;
        if (noun != null && noun.length() > 0) {
            className = noun.substring(0, 1).toUpperCase();
            if (noun.length() > 1) {
                className += noun.substring(1, noun.length());
            }
        }
        return className;
    }
    
    /**
     * Field names are derived from property names by lower casing the
     * first character.
     * @param propertyName the property name
     * @return a valid field name or null if property name is empty
     */
    public static String fieldNameFromPropertyName (final String propertyName) {
    	String fieldName = null;
    	if (propertyName != null && propertyName.length() > 0) {
    		fieldName = propertyName.substring(0, 1).toLowerCase();
    		if (propertyName.length() > 1) {
    			fieldName += propertyName.substring(1, propertyName.length());
    		}
    	}
    	return fieldName;
    }

    /**
     * Property names are derived from field names by upper casing the
     * first character.
     * @param fieldName the field name
     * @return a valid property name or null if field name is empty
     */
    public static String propertyNameFromFieldName (final String fieldName) {
    	String propertyName = null;
    	if (fieldName != null && fieldName.length() > 0) {
    		propertyName = fieldName.substring(0, 1).toUpperCase();
    		if (fieldName.length() > 1) {
    			propertyName += fieldName.substring(1, fieldName.length());
    		}
    	}
    	return propertyName;
    }

    /**
     * Property names are derived from jaxb type names by stripping the
     * type suffix (if any).
     * @param jaxbType the jaxb type name
     * @return a valid property name or null if jaxb type name is empty
     */
    public static String propertyNameFromJaxbType (final String jaxbType) {
    	String propertyName = null;
    	if (jaxbType != null && jaxbType.length() > 0) {
            propertyName = jaxbType;
            if (propertyName.endsWith(JAXB_TYPE_SUFFIX)) {
            	propertyName = propertyName.substring(0,
            			propertyName.length() - JAXB_TYPE_SUFFIX.length());
            }
    	}
    	return propertyName;
    }
}
