/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;

/**
 * A helper class for dynamic class loading.
 * 
 */
public final class ClassUtil {

    /** Utility class. */
    private ClassUtil() {

    }

    /**
     * Rather than using the Class.forName mechanism first, this uses
     * Thread.getContextClassLoader instead. In a Servlet context such as
     * Tomcat, this allows JAXB classes for instance to be loaded from the web
     * application (webapp) location while this code might have been loaded from
     * shared/lib. If Thread.getContextClassLoader fails to locate the class
     * then we give a last chance to Class.forName.
     * 
     * @param packageName the package containing the class
     * @param className the class name
     * @return the class
     * @throws ClassNotFoundException if class is not accessible from any class
     *             loader
     */
    public static Class < ? > loadClass(final String packageName,
            final String className) throws ClassNotFoundException {
        return loadClass(toQualifiedClassName(packageName, className));
    }

    /**
     * Rather than using the Class.forName mechanism first, this uses
     * Thread.getContextClassLoader instead. In a Servlet context such as
     * Tomcat, this allows JAXB classes for instance to be loaded from the web
     * application (webapp) location while this code might have been loaded from
     * shared/lib. If Thread.getContextClassLoader fails to locate the class
     * then we give a last chance to Class.forName.
     * 
     * @param qualifiedClassName the class name to load
     * @return the class
     * @throws ClassNotFoundException if class is not accessible from any class
     *             loader
     */
    public static Class < ? > loadClass(final String qualifiedClassName)
            throws ClassNotFoundException {
        ClassLoader contextClassLoader = Thread.currentThread()
                .getContextClassLoader();
        if (contextClassLoader == null) {
            return Class.forName(qualifiedClassName);
        }
        try {
            return contextClassLoader.loadClass(qualifiedClassName);
        } catch (ClassNotFoundException e) {
            return Class.forName(qualifiedClassName);
        }
    }

    /**
     * Dynamically create an instance of a java class. This assumes the classes
     * are available on the classpath and returns a new instance.
     * 
     * @param packageName the package containing the class
     * @param className the class name
     * @return a java object
     * @throws ClassLoadingException if object cannot be instantiated
     */
    public static Object newObject(final String packageName,
            final String className) throws ClassLoadingException {
        return newObject(toQualifiedClassName(packageName, className));

    }

    /**
     * Dynamically create an instance of a java class. This assumes the classes
     * are available on the classpath and returns a new instance.
     * 
     * @param qualifiedClassName the qualified class name
     * @return a java object
     * @throws ClassLoadingException if object cannot be instantiated
     */
    public static Object newObject(final String qualifiedClassName)
            throws ClassLoadingException {

        try {
            Class < ? > clazz = loadClass(qualifiedClassName);
            return clazz.newInstance();
        } catch (InstantiationException e) {
            throw new ClassLoadingException(e);
        } catch (IllegalAccessException e) {
            throw new ClassLoadingException(e);
        } catch (ClassNotFoundException e) {
            throw new ClassLoadingException(e);
        }
    }

    /**
     * Qualifies a class name.
     * 
     * @param packageName the package name, null if none
     * @param className the class name
     * @return a qualified class name
     */
    public static String toQualifiedClassName(final String packageName,
            final String className) {
        return (packageName == null) ? className : packageName + '.'
                + className;
    }

    /**
     * Separate package name and class name from a qualified class name.
     * 
     * @param qualifiedClassName qualified class name
     * @return a structure with both parts
     */
    public static ClassName toClassName(final String qualifiedClassName) {
        ClassName className = new ClassName();
        int pos = qualifiedClassName.lastIndexOf('.');
        className.packageName = (pos == -1) ? null : qualifiedClassName
                .substring(0, pos);
        className.className = (pos == -1) ? qualifiedClassName
                : qualifiedClassName.substring(pos + 1);
        return className;
    }

    /**
     * A simple vehicle for class name and package name.
     * 
     */
    public static class ClassName {
        /** The class name. */
        public String className;
        /** The package name (null if no package name). */
        public String packageName;
    }

    /**
     * The ObjectFactory exports a create method for each complex type in the
     * object tree. Returns a jaxb object creator method.
     * 
     * @param objectFactory JAXB Objectfactory
     * @param jaxbTypeName the jaxb object type name
     * @return the creator method
     * @throws ClassMethodException if getter method does not exist
     */
    public static Method getCreatorMethod(final Object objectFactory,
            final String jaxbTypeName) throws ClassMethodException {
        try {
            Method creator = objectFactory.getClass().getMethod(
                    "create" + toPropertyType(jaxbTypeName));
            return creator;
        } catch (NoSuchMethodException e) {
            throw (new ClassMethodException(e));
        }
    }

    /**
     * Returns a jaxb property getter method.
     * 
     * @param parentObject JAXB Object owning the property
     * @param getterPrefix the getter method prefix (get or is)
     * @param jaxbName the property name to get
     * @return the getter method
     * @throws ClassMethodException if getter method does not exist
     */
    public static Method getGetterMethod(final Object parentObject,
            final String getterPrefix, final String jaxbName)
            throws ClassMethodException {
        String getterName = getterPrefix + NameUtil.upperFirstChar(jaxbName)
                + jaxbName.substring(1);
        try {
            Method getter = parentObject.getClass().getMethod(getterName);
            return getter;
        } catch (NoSuchMethodException e) {
            throw (new ClassMethodException(e));
        }
    }

    /**
     * Returns a jaxb property setter method.
     * 
     * @param parentObject JAXB Object owning the property
     * @param jaxbName the property name to get
     * @param jaxbType the property type
     * @return the getter method
     * @throws ClassMethodException if getter method does not exist
     */
    public static Method getSetterMethod(final Object parentObject,
            final String jaxbName, final Class < ? > jaxbType)
            throws ClassMethodException {
        String getterName = "set" + NameUtil.upperFirstChar(jaxbName)
                + jaxbName.substring(1);
        try {
            Class < ? >[] param = { jaxbType };
            Method setter = parentObject.getClass()
                    .getMethod(getterName, param);
            return setter;
        } catch (NoSuchMethodException e) {
            throw (new ClassMethodException(e));
        }
    }

    /**
     * Helper method to extract the type name as used by JAXB create methods
     * Property type (as returned from Field.getGenericType() can take 3 forms:
     * form1: type form1: package.type form3: java.util.List &lts; package.type
     * &gt;. Furthermore, primitive types are returned as their Object
     * equivalent.
     * 
     * 
     * @param genericType as returned from hostField.getGenericType()
     * @return a normalized string representation of the type
     */
    public static String toPropertyType(final String genericType) {

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
        } else if (type.compareTo("boolean") == 0) {
            return "Boolean";
        }
        return type;
    }

    /**
     * This method gets a property object from a parent object.
     * 
     * @param parentObject JAXB Object owning the property
     * @param jaxbName the property name to get
     * @return instance of the property as returned from a getter method
     * @throws ClassInvokeException if property cannot be accessed
     * @deprecated use {@link invokeGetProperty(Object parentObject, String
     *             jaxbName, Class < ? > jaxbType)}
     */
    public static Object invokeGetProperty(final Object parentObject,
            final String jaxbName) throws ClassInvokeException {
        Object result;
        try {
            /*
             * The concrete object reference is obtained thru the corresponding
             * parent getter method
             */
            Method getter = getGetterMethod(parentObject, "get", jaxbName);
            result = getter.invoke(parentObject);
        } catch (IllegalAccessException e) {
            throw (new ClassInvokeException(e));
        } catch (InvocationTargetException e) {
            throw (new ClassInvokeException(e));
        } catch (ClassMethodException e) {
            throw (new ClassInvokeException(e));
        }
        return result;
    }

    /**
     * This method gets a property object from a parent object.
     * 
     * @param parentObject JAXB Object owning the property
     * @param jaxbName the property name to get
     * @param maxOccurs the maximum number of occurrences
     * @return instance of the property as returned from a getter method
     * @throws ClassInvokeException if property cannot be accessed
     */
    public static Object invokeGetProperty(final Object parentObject,
            final String jaxbName, final Class < ? > jaxbType,
            final int maxOccurs) throws ClassInvokeException {
        Object result;
        try {
            /*
             * The concrete object reference is obtained thru the corresponding
             * parent getter method
             */
            Method getter = getGetterMethod(parentObject,
                    getGetterPrefix(jaxbType, maxOccurs), jaxbName);
            result = getter.invoke(parentObject);
        } catch (IllegalAccessException e) {
            throw (new ClassInvokeException(e));
        } catch (InvocationTargetException e) {
            throw (new ClassInvokeException(e));
        } catch (ClassMethodException e) {
            throw (new ClassInvokeException(e));
        }
        return result;
    }

    /**
     * Evaluates the getter method prefix for a field, based on the field type.
     * 
     * @param fieldType the field type
     * @param maxOccurs the maximum number of occurrences
     * @return the prefix to form a getter method
     */
    public static String getGetterPrefix(final Class < ? > fieldType,
            final int maxOccurs) {
        String getterPrefix = "get";
        if (maxOccurs <= 1) {
            if (fieldType == boolean.class) {
                getterPrefix = "is";
            } else if (fieldType == Boolean.class) {
                getterPrefix = "is";
            }
        }
        return getterPrefix;
    }

    /**
     * This method sets a property from a parent object to a given value.
     * 
     * @param parentObject JAXB Object owning the property
     * @param jaxbName the property name to set
     * @param value value to set property to
     * @param javaType the java class the value belongs to
     * @throws ClassInvokeException if property cannot be set
     */
    @SuppressWarnings({ "rawtypes" })
    public static void invokeSetProperty(final Object parentObject,
            final String jaxbName, final Object value,
            final Class < ? > javaType) throws ClassInvokeException {

        try {
            /*
             * When object to set is a List, we use the getXXX().AddAll
             * technique
             */
            if (value instanceof List) {
                Method getter = getGetterMethod(parentObject, "get", jaxbName);
                Class < ? > listClass = getter.getReturnType();
                Method addAll = listClass.getMethod("addAll", Collection.class);
                addAll.invoke(getter.invoke(parentObject), (Collection) value);
            } else {
                Method setter = getSetterMethod(parentObject, jaxbName,
                        javaType);
                setter.invoke(parentObject, value);
            }
        } catch (IllegalAccessException e) {
            throw (new ClassInvokeException(e));
        } catch (NoSuchMethodException e) {
            throw (new ClassInvokeException(e));
        } catch (InvocationTargetException e) {
            throw (new ClassInvokeException(e));
        } catch (ClassMethodException e) {
            throw (new ClassInvokeException(e));
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
     * @throws ClassInvokeException if JAXB Object cannot be instanciated
     */
    public static Object addComplexProperty(final Object objectFactory,
            final Object parentObject, final String jaxbName,
            final String jaxbTypeName) throws ClassInvokeException {

        try {
            Method creator = getCreatorMethod(objectFactory, jaxbTypeName);
            Object result = creator.invoke(objectFactory);

            /*
             * Add a reference to the object just created to the parent object
             * using its setter method
             */
            Method setter = getSetterMethod(parentObject, jaxbName,
                    creator.getReturnType());
            setter.invoke(parentObject, result);
            return result;
        } catch (IllegalAccessException e) {
            throw new ClassInvokeException(e);
        } catch (InvocationTargetException e) {
            throw new ClassInvokeException(e);
        } catch (ClassMethodException e) {
            throw new ClassInvokeException(e);
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
     * @throws ClassInvokeException if JAXB Object cannot be returned
     */
    public static Object invokeGetIndexedProperty(final Object parentObject,
            final int index) throws ClassInvokeException {

        Object result;
        try {
            /*
             * To get an instance of the item identified by index, we use the
             * get(i) method of ArrayList
             */
            Method getter = parentObject.getClass().getMethod("get", int.class);
            result = getter.invoke(parentObject, index);
        } catch (IllegalAccessException e) {
            throw new ClassInvokeException(e);
        } catch (NoSuchMethodException e) {
            throw new ClassInvokeException(e);
        } catch (InvocationTargetException e) {
            /* If requested item does not exist in array simply return null */
            if (e.getTargetException().getClass().getName()
                    .compareTo("java.lang.IndexOutOfBoundsException") == 0) {
                return null;
            } else {
                throw new ClassInvokeException(e);
            }
        }
        return result;
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
     * @throws ClassInvokeException if JAXB Object cannot be instanciated
     */
    public static Object addComplexIndexedProperty(final Object objectFactory,
            final Object parentObject, final String jaxbTypeName,
            final int index) throws ClassInvokeException {

        try {
            Method creator = getCreatorMethod(objectFactory, jaxbTypeName);
            Object result = creator.invoke(objectFactory);

            /*
             * Add reference to object just created to the parent array using
             * the add(index, Object) method
             */
            Class < ? >[] param = { int.class, Object.class };
            Method add = parentObject.getClass().getMethod("add", param);
            add.invoke(parentObject, index, result);
            return result;
        } catch (IllegalAccessException e) {
            throw new ClassInvokeException(e);
        } catch (NoSuchMethodException e) {
            throw new ClassInvokeException(e);
        } catch (InvocationTargetException e) {
            throw new ClassInvokeException(e);
        } catch (ClassMethodException e) {
            throw new ClassInvokeException(e);
        }
    }

    /**
     * This creates a new simple property item, identified by an index in a
     * parent object array.
     * 
     * @param parentObject instance of the JAXB parent list class
     * @param index item index in list
     * @param value value to set item to
     * @throws ClassInvokeException if item cannot be set
     */
    public static void addSimpleIndexedProperty(final Object parentObject,
            final int index, final Object value) throws ClassInvokeException {

        try {
            /*
             * Add reference to object just created to the parent array using
             * the add(index, Object) method
             */
            Class < ? >[] param = { int.class, Object.class };
            Method setter = parentObject.getClass().getMethod("add", param);
            setter.invoke(parentObject, index, value);
        } catch (IllegalAccessException e) {
            throw new ClassInvokeException(e);
        } catch (NoSuchMethodException e) {
            throw new ClassInvokeException(e);
        } catch (InvocationTargetException e) {
            throw new ClassInvokeException(e);
        }
    }

    /**
     * This will determine if a type name for a simple element is one of java's
     * native types and assume it is an enum otherwise.
     * 
     * @param type the element java type
     * @return true if it should be considered an enum
     */
    public static boolean isEnum(final String type) {
        if (type == null) {
            return false;
        }
        if (type.compareToIgnoreCase("String") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("[B") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("byte[]") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("short") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("int") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("Integer") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("long") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("float") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("double") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("BigInteger") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("BigDecimal") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("byte") == 0) {
            return false;
        } else if (type.compareToIgnoreCase("boolean") == 0) {
            return false;
        }
        return true;
    }

}
