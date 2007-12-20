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
package com.legstar.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;

import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.host.HostException;

/**
 * Utility methods used for JAXB object tree introspection.
 * 
 * @author Fady Moussallam
 * 
 *
 */
public final class JaxbUtil {
	
	/** Suffix for binding class name. */
	private static final String BIND_SUFFIX = "Binding";
	
	/** Suffix for array wrappers class names. */
	private static final String WRAPPER_SUFFIX = "Wrapper";
	
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
			Method getter = getGetterMethod(parentObject, jaxbName);
			result = getter.invoke(parentObject);
		} catch (IllegalAccessException e) {
			throw (new HostException(e));
		} catch (InvocationTargetException e) {
			throw (new HostException(e));
		}
		return result;
	}
	
	/**
	 * This method sets a property from a parent object to a given value.
	 * 
	 * @param parentObject JAXB Object owning the property
	 * @param jaxbName the property name to set
	 * @param value value to set property to
	 * @param javaType the java class the value belongs to
	 * @throws HostException if property cannot be set
	 */
	@SuppressWarnings("unchecked")
	public static void invokeSetProperty(
			final Object parentObject,
			final String jaxbName,
			final Object value,
			final Class javaType)
		throws HostException {
		
		try {
			/* When object to set is a List, we use the getXXX().AddAll
			 *  technique*/
			if (value instanceof List) {
				Method getter = getGetterMethod(parentObject, jaxbName);
				Class listClass = getter.getReturnType();
				Method addAll =  listClass.getMethod(
						"addAll", Collection.class);
				addAll.invoke(getter.invoke(parentObject), (Collection) value);
			} else {
				Method setter = getSetterMethod(
						parentObject, jaxbName, javaType);
				setter.invoke(parentObject, value);
			}
		} catch (IllegalAccessException e) {
			throw (new HostException(e));
		} catch (NoSuchMethodException e) {
			throw (new HostException(e));
    	} catch (InvocationTargetException e) {
			throw (new HostException(e));
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
	 * @param jaxbTypeName the complex type name to create
	 * @return an instance of a JAXB object
	 * @throws HostException if JAXB Object cannot be instanciated
	 */
	public static Object createComplexProperty(
			final Object objectFactory,
			final String jaxbTypeName)
		throws HostException {
		
		try {
			Method creator = getCreatorMethod(objectFactory, jaxbTypeName);
			return creator.invoke(objectFactory);
		} catch (IllegalAccessException e) {
			throw (new HostException(e));
		} catch (InvocationTargetException e) {
			throw (new HostException(e));
		}
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
	 * @param jaxbTypeName complex property type name
	 * @return an instance of a JAXB object
	 * @throws HostException if JAXB Object cannot be instanciated
	 */
	public static Object addComplexProperty(
			final Object objectFactory,
			final Object parentObject,
			final String jaxbName,
			final String jaxbTypeName)
		throws HostException {
		
		try {
			Method creator = getCreatorMethod(objectFactory, jaxbTypeName);
			Object result = creator.invoke(objectFactory);
			
			/* Add a reference to the object just created to the parent object
			 * using its setter method */
			Method setter = getSetterMethod(
					parentObject, jaxbName, creator.getReturnType());
			setter.invoke(parentObject, result);
			return result;
		} catch (IllegalAccessException e) {
			throw (new HostException(e));
		} catch (InvocationTargetException e) {
			throw (new HostException(e));
		}
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
			throw (new HostException(e));
		} catch (NoSuchMethodException e) {
			throw (new HostException(e));
		} catch (InvocationTargetException e) {
			/* If requested item does not exist in array simply return null */
			if (e.getTargetException().getClass().getName().compareTo(
					"java.lang.IndexOutOfBoundsException") == 0) {
				return null;
			} else {
				throw (new HostException(e));
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
			throw (new HostException(e));
		} catch (NoSuchMethodException e) {
			throw (new HostException(e));
    	} catch (InvocationTargetException e) {
			throw (new HostException(e));
    	}
	}

	/**
	 * This creates a new complex property item, identified by an index in a
	 * parent object array. This method assumes the items are complex objects
	 * for which the JAXB ObjectFactory implements a create method.
	 * 
	 * @param objectFactory a JAXB ObjectFactory
	 * @param parentObject instance of the JAXB parent class
	 * @param jaxbTypeName complex property type name
	 * @param index item index in list
	 * @return an instance of a JAXB object
	 * @throws HostException if JAXB Object cannot be instanciated
	 */
	public static Object addComplexIndexedProperty(
			final Object objectFactory,
			final Object parentObject,
			final String jaxbTypeName,
			final int index)
		throws HostException {
		
		try {
			Method creator = getCreatorMethod(objectFactory, jaxbTypeName);
			Object result = creator.invoke(objectFactory);
			
			/* Add reference to object just created to the parent array using
			 * the add(index, Object) method */
			Class[] param = {int.class, Object.class};
			Method add = parentObject.getClass().getMethod("add", param);
			add.invoke(parentObject, index, result);
			return result;
		} catch (IllegalAccessException e) {
			throw (new HostException(e));
		} catch (NoSuchMethodException e) {
    		throw (new HostException(e));
    	} catch (InvocationTargetException e) {
    		throw (new HostException(e));
    	}
	}
	
	/**
	 * This function returns a byte length for a complex object as a String.
	 * 
	 * @param jaxbPackage the java package name from which an ObjectFactory
	 *        can be instanciated
	 * @param jaxbTypeName the JAXB type name of the object for which byte 
	 *        length must be returned
	 * @return the byte length as a string
	 * @throws HostException if byte length calculation failed
	 */
	public static String byteLength(
			final String jaxbPackage,
			final String jaxbTypeName)
		throws HostException {
		
		try {
			/* Load the JAXB object factory from the package */
			String ofName = jaxbPackage + ".ObjectFactory";
			Class cl = Class.forName(ofName);
			Object objectFactory = cl.newInstance();
			
			/* Get an instance of the requested JAXB object*/
			Object jaxbObject =
				JaxbUtil.createComplexProperty(objectFactory, jaxbTypeName);
			
			/* Create a complex cobol element representing this jaxb object */
			CComplexReflectBinding ce =
				new CComplexReflectBinding(objectFactory, jaxbObject);
			return (new Integer(ce.calcByteLength())).toString();
		} catch (ClassNotFoundException e) {
			throw (new HostException(e));
		} catch (InstantiationException e) {
			throw (new HostException(e));
		} catch (IllegalAccessException e) {
			throw (new HostException(e));
		}
	}

	/**
	 * The ObjectFactory exports a create method for each complex type
	 * in the object tree.
	 * Returns a jaxb object creator method.
	 * @param objectFactory JAXB Objectfactory
	 * @param jaxbTypeName the jaxb object type name
	 * @return the creator method
	 * @throws HostException if getter method does not exist
	 */
	public static Method getCreatorMethod(
			final Object objectFactory,
			final String jaxbTypeName) throws HostException {
		try {
			Method creator =
				objectFactory.getClass().getMethod(
						"create" + toPropertyType(jaxbTypeName));
			return creator;
		} catch (NoSuchMethodException e) {
			throw (new HostException(e));
		}
	}

	/**
	 * Returns a jaxb property getter method.
	 * @param parentObject JAXB Object owning the property
	 * @param jaxbName the property name to get
	 * @return the getter method
	 * @throws HostException if getter method does not exist
	 */
	public static Method getGetterMethod(
			final Object parentObject,
			final String jaxbName) throws HostException {
		String getterName = "get" + jaxbName.substring(0, 1).toUpperCase()
		+ jaxbName.substring(1);
		try {
			Method getter = parentObject.getClass().getMethod(getterName);
			return getter;
		} catch (NoSuchMethodException e) {
			throw (new HostException(e));
		}
	}

	/**
	 * Returns a jaxb property setter method.
	 * @param parentObject JAXB Object owning the property
	 * @param jaxbName the property name to get
	 * @param jaxbType the property type
	 * @return the getter method
	 * @throws HostException if getter method does not exist
	 */
	public static Method getSetterMethod(
			final Object parentObject,
			final String jaxbName,
			final Class jaxbType) throws HostException {
		String getterName = "set" + jaxbName.substring(0, 1).toUpperCase()
		+ jaxbName.substring(1);
		try {
			Class[] param = {jaxbType};
			Method setter = parentObject.getClass().getMethod(
					getterName, param);
			return setter;
		} catch (NoSuchMethodException e) {
			throw (new HostException(e));
		}
	}

	/**
	 * Helper method to extract the type name as used by JAXB create methods	
	 * Property type (as returned from Field.getGenericType() can take 3 forms: 
	 * form1: type
	 * form1: package.type
	 * form3: java.util.List &lts; package.type &gt;.
	 * Furthermore, primitive types are returned as their Object equivalent.
	 * 
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
		if (type.compareTo("byte") == 0) {
			return "Byte";
		} else if (type.compareTo("[B") == 0) {
			return "byte[]";
		} else if (type.compareTo("short") == 0) {
			return "Short";
		} else if (type.compareTo("int") == 0) {
			return "Integer";
		} else if (type.compareTo("long") == 0) {
			return "Long";
		} else if (type.compareTo("float") == 0) {
			return "Float";
		} else if (type.compareTo("double") == 0) {
			return "Double";
		}
		return type;
	}
	
	/**
	 * Returns the binding associated jaxb type name. Since certain bindings
	 * are not associated with a jaxb property, this might return null.
	 * @param binding the binding for which the jaxb type is to be returned
	 * @return the binding associated jaxb type name
	 */
	public static String getJaxbTypeName(
			final ICobolBinding binding) {
		if (binding.getJaxbType() == null) {
			return null;
		}
		return JaxbUtil.toPropertyType(binding.getJaxbType().getName());
	}
	
	/**
	 * Builds a binding type name using the associated jaxb type name.
	 * @param binding the binding for which the binding type is to be returned
	 * @return the binding type name
	 */
	public static String getCoxbTypeName(
			final ICobolBinding binding) {
		/* If there is no bound jaxb object, the name of the binding type is
		 * built from the binding name */
		if (binding.getJaxbType() == null) {
            return binding.getBindingName().substring(0, 1).toUpperCase()
				+ binding.getBindingName().substring(
						1, binding.getBindingName().length())
				+ BIND_SUFFIX;
		}
		
		/* Arrays of complex bindings are returned as wrappers */
		if (binding instanceof ICobolArrayComplexBinding) {
			return JaxbUtil.getJaxbTypeName(binding)
			+ WRAPPER_SUFFIX + BIND_SUFFIX;
		}
		return JaxbUtil.getJaxbTypeName(binding) + BIND_SUFFIX;
	}

	/**
	 * Returns a name that can be used as a field name.
	 * @param binding the binding for which the field name is to be returned
	 * @return the binding proposed field name
	 */
	public static String getFieldName(
			final ICobolBinding binding) {
        return binding.getBindingName().substring(0, 1).toLowerCase()
		+ binding.getBindingName().substring(
				1, binding.getBindingName().length());
	}
	
}
