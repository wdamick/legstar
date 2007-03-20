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
package com.legstar.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import com.legstar.coxb.reflect.CComplexBinding;
import com.legstar.host.HostException;

/**
 * Utility methods used for JAXB object tree introspection.
 * 
 * @author Fady Moussallam
 * 
 *
 */
public final class JaxbUtil {
	
	/**
	 * Private constructor to stop anyone from instantiating
	 * this class - the static methods should be used
	 * explicitly.
	 */
	private JaxbUtil()	{
		
	}
	
	/** 
	 * This method gets a property object from a parent object.
	 * 
	 * @param parentObject JAXB Object owning the property
	 * @param jaxbName the property name to get
	 * @return instance of the property as returned from a getter method
	 * @throws HostException if property cannot be accessed
	 */
	public static Object invokeGetProperty(
			final Object parentObject,
			final String jaxbName)
		throws HostException {
		Object result;
		try {
			/* The concrete object reference is obtained thru the corresponding
			 *  parent getter method */
			String getterName = "get" + jaxbName.substring(0, 1).toUpperCase()
			+ jaxbName.substring(1);
			Method getter = parentObject.getClass().getMethod(getterName);
			result = getter.invoke(parentObject);
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
			throw (new HostException(
					"NoSuchMethodException " + e.getMessage()));
		} catch (InvocationTargetException e) {
			throw (new HostException(
					"InvocationTargetException " + e.getMessage()));
		}
		return result;
	}

	/**
	 * This method sets a property from a parent object to a given value.
	 * 
	 * @param parentObject JAXB Object owning the property
	 * @param jaxbName the property name to set
	 * @param value value to set property to
	 * @param clazz the java class the value belongs to
	 * @throws HostException if property cannot be set
	 */
	public static void invokeSetProperty(
			final Object parentObject,
			final String jaxbName,
			final Object value,
			final Class clazz)
		throws HostException {
		
		try {
			Class[] param = {clazz};
			String setterName = "set" + jaxbName.substring(0, 1).toUpperCase()
			+ jaxbName.substring(1);
			Method setter =
				parentObject.getClass().getMethod(setterName, param);
			setter.invoke(parentObject, value);
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
    		throw (new HostException(
    				"NoSuchMethodException " + e.getMessage()));
    	} catch (InvocationTargetException e) {
    		throw (new HostException(
    				"InvocationTargetException " + e.getMessage()));
    	}
	}

	/** 
	 * This method creates a new instance of a property type using a JAXB
	 * ObjectFactory. Given the property type name, the method constructs an
	 * ObjectFactory call for this property type. This method works for complex
	 * types only since the ObjectFactory will have create methods only for
	 * those.
	 * 
	 * @param objectFactory a JAXB ObjectFactory
	 * @param jaxbType the complex type name to create
	 * @return an instance of a JAXB object
	 * @throws HostException if JAXB Object cannot be instanciated
	 */
	public static Object createComplexProperty(
			final Object objectFactory,
			final String jaxbType)
		throws HostException {
		
		Object result;
		try {
			/* The ObjectFactory exports a create method for each complex type
			 * in the object tree */
			Method getter =
				objectFactory.getClass().getMethod(
						"create" + toPropertyType(jaxbType));
			result = getter.invoke(objectFactory);
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
			throw (new HostException(
					"NoSuchMethodException " + e.getMessage()));
		} catch (InvocationTargetException e) {
			throw (new HostException(
					"InvocationTargetException " + e.getMessage()));
		}
		return result;
	}

	/**
	 * This method creates a new instance of a property type in a parent object
	 * using a JAXB ObjectFactory. Given the property type name, the method
	 * constructs an ObjectFactory call for this property type. This method
	 * works for complex types only since the ObjectFactory will have create 
	 * methods only for those.
	 * 
	 * @param objectFactory a JAXB ObjectFactory
	 * @param parentObject instance of the JAXB parent class
	 * @param jaxbName complex property name
	 * @param jaxbType complex property type
	 * @return an instance of a JAXB object
	 * @throws HostException if JAXB Object cannot be instanciated
	 */
	public static Object addComplexProperty(
			final Object objectFactory,
			final Object parentObject,
			final String jaxbName,
			final String jaxbType)
		throws HostException {
		
		Object result;
		try {
			/* The ObjectFactory exports a create method for each complex type
			 *  in the object tree */
			Method getter =
				objectFactory.getClass().getMethod(
						"create" + toPropertyType(jaxbType));
			result = getter.invoke(objectFactory);
			
			/* Add a reference to the object just created to the parent object
			 * using its setter method */
			Class[] param = {getter.getReturnType()};
			String setterName = "set" + jaxbName.substring(0, 1).toUpperCase()
			+ jaxbName.substring(1);
			Method setter =
				parentObject.getClass().getMethod(setterName, param);
			setter.invoke(parentObject, result);
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
			throw (new HostException(
					"NoSuchMethodException " + e.getMessage()));
		} catch (InvocationTargetException e) {
			throw (new HostException(
					"InvocationTargetException " + e.getMessage()));
		}
		return result;
	}

	/**
	 * This method gets an indexed property object from a parent array object. 
	 * Assumes the parent inherits from java.util.List and implement a
	 * get(index) method.
	 * 
	 * @param parentObject instance of the JAXB parent list class
	 * @param index item index in list
	 * @return an instance of a JAXB object
	 * @throws HostException if JAXB Object cannot be returned
	 */
	public static Object invokeGetIndexedProperty(
			final Object parentObject,
			final int index)
		throws HostException {
		
		Object result;
		try {
			/* To get an instance of the item identified by index, we use the
			 *  get(i) method of ArrayList */
			Method getter = parentObject.getClass().getMethod("get", int.class);
			result = getter.invoke(parentObject, index);
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
			throw (new HostException(
					"NoSuchMethodException " + e.getMessage()));
		} catch (InvocationTargetException e) {
			/* If requested item does not exist in array simply return null */
			if (e.getTargetException().getClass().getName().compareTo(
					"java.lang.IndexOutOfBoundsException") == 0) {
				return null;
			} else {
				throw (new HostException(
						"InvocationTargetException " + e.getMessage()));
			}
		}
		return result;
	}
	
	/** 
	 * This creates a new simple property item, identified by an index in a
	 * parent object array. 
	 * 
	 * @param parentObject instance of the JAXB parent list class
	 * @param index item index in list
	 * @param value value to set item to
	 * @throws HostException if item cannot be set
	 */
	public static void addSimpleIndexedProperty(
			final Object parentObject,
			final int index,
			final Object value)
		throws HostException {
		
		try {
			/* Add reference to object just created to the parent array using
			 *  the add(index, Object) method */
			Class[] param = {int.class, Object.class};
			Method setter = parentObject.getClass().getMethod("add", param);
			setter.invoke(parentObject, index, value);
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
    		throw (new HostException(
    				"NoSuchMethodException " + e.getMessage()));
    	} catch (InvocationTargetException e) {
    		throw (new HostException(
    				"InvocationTargetException " + e.getMessage()));
    	}
	}

	/**
	 * This creates a new complex property item, identified by an index in a
	 * parent object array. This method assumes the items are complex objects
	 * for which the JAXB ObjectFactory implements a create method.
	 * 
	 * @param objectFactory a JAXB ObjectFactory
	 * @param parentObject instance of the JAXB parent class
	 * @param jaxbType complex property type
	 * @param index item index in list
	 * @return an instance of a JAXB object
	 * @throws HostException if JAXB Object cannot be instanciated
	 */
	public static Object addComplexIndexedProperty(
			final Object objectFactory,
			final Object parentObject,
			final String jaxbType,
			final int index)
		throws HostException {
		
		Object result;
		try {
			/* The ObjectFactory exports a create method for each complex type
			 * in the object tree */
			Method getter =
				objectFactory.getClass().getMethod(
						"create" + toPropertyType(jaxbType));
			result = getter.invoke(objectFactory);
			
			/* Add reference to object just created to the parent array using
			 * the add(index, Object) method */
			Class[] param = {int.class, Object.class};
			Method setter = parentObject.getClass().getMethod("add", param);
			setter.invoke(parentObject, index, result);
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException " + e.getMessage()));
		} catch (NoSuchMethodException e) {
    		throw (new HostException(
    				"NoSuchMethodException " + e.getMessage()));
    	} catch (InvocationTargetException e) {
    		throw (new HostException(
    				"InvocationTargetException " + e.getMessage()));
    	}
		return result;
	}
	
	/**
	 * This function returns a byte length for a complex object as a String.
	 * 
	 * @param jaxbPackage the java package name from which an ObjectFactory
	 *        can be instanciated
	 * @param jaxbType the JAXB type of the object for which byte length must
	 *        be returned
	 * @return the byte length as a string
	 * @throws HostException if byte length calculation failed
	 */
	public static String byteLength(
			final String jaxbPackage,
			final String jaxbType)
		throws HostException {
		
		String result = null;
		try {
			/* Load the JAXB object factory from the package */
			String ofName = jaxbPackage + ".ObjectFactory";
			Class cl = Class.forName(ofName);
			Object objectFactory = cl.newInstance();
			
			/* Get an instance of the requested JAXB object*/
			Object jaxbObject =
				JaxbUtil.createComplexProperty(objectFactory, jaxbType);
			
			/* Create a complex cobol element representing this jaxb object */
			CComplexBinding ce =
				new CComplexBinding(objectFactory, jaxbObject);
			result = (new Integer(ce.getByteLength())).toString();
		} catch (ClassNotFoundException e) {
			throw (new HostException(
					"ClassNotFoundException: " + e.getMessage()));
		} catch (InstantiationException e) {
			throw (new HostException(
					"InstantiationException: " + e.getMessage()));
		} catch (IllegalAccessException e) {
			throw (new HostException(
					"IllegalAccessException: " + e.getMessage()));
		}
		return result;
	}

	/**
	 * Helper method to extract the type name as used by JAXB create methods	
	 * Property type (as returned from Field.getGenericType() can take 3 forms: 
	 * form1: type
	 * form1: package.type
	 * form3: java.util.List < package.type >.
	 * 
	 * @param genericType as returned from hostField.getGenericType()
	 * @return a normalized string representation of the type
	 */
	public static String toPropertyType(
			final String genericType) {
		
		String type;
		int lp = genericType.lastIndexOf('.');
		if (lp == -1) {
			type = genericType;
		} else {
			type = genericType.substring(lp + 1);
			if (type.charAt(type.length() - 1) == '>') {
				type = type.substring(0, type.length() - 1);
			}
		}
		return type;
	}
}
