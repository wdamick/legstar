/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.util;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.transform.IHostTransformers;
import com.legstar.coxb.util.ClassUtil.ClassName;

/**
 * Helper methods useful when manipulating bindings and transformers directly.
 * 
 */
public final class BindingUtil {

    /** Suffix for binding class name. */
    public static final String BIND_SUFFIX = "Binding";

    /** Suffix for array wrappers class names. */
    public static final String WRAPPER_SUFFIX = "Wrapper";

    /** The JAXB object factory name. */
    public static final String JAXB_OBJECTFACTORY_NAME = "ObjectFactory";

    /** Binding classes go to a package which name ends with this. */
    public static final String COXB_PACKAGENAME_SUFFIX = "bind";

    /** Utility class. */
    private BindingUtil() {

    }

    /**
     * Search for a binding with a specific name.
     * <p/>
     * This will descend the tree represented by bindings and their children
     * until it finds a binding with the requested name.
     * 
     * @param rootBinding the starting point for the search
     * @param bindingName the
     * @return the binding with the requested name or null if none is found
     * @throws HostException if no binding exist
     */
    public static ICobolBinding lookupBinding(final ICobolBinding rootBinding,
            final String bindingName) throws HostException {
        if (rootBinding.getBindingName().equals(bindingName)) {
            return rootBinding;
        }
        if (rootBinding instanceof ICobolComplexBinding) {
            for (ICobolBinding childBinding : ((ICobolComplexBinding) rootBinding)
                    .getChildrenList()) {
                ICobolBinding result = lookupBinding(childBinding, bindingName);
                if (result != null) {
                    return result;
                }
            }
        }
        if (rootBinding instanceof ICobolArrayComplexBinding) {
            return lookupBinding(
                    ((ICobolArrayComplexBinding) rootBinding)
                            .getComplexItemBinding(),
                    bindingName);
        }
        if (rootBinding instanceof ICobolChoiceBinding) {
            for (ICobolBinding childBinding : ((ICobolChoiceBinding) rootBinding)
                    .getAlternativesList()) {
                ICobolBinding result = lookupBinding(childBinding, bindingName);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;

    }

    /**
     * Loads a JAXB object factory class using a combination of current and
     * thread class loader. This assumes the JAXB classes are available on the
     * classpath and returns a new instance of of the object factory.
     * 
     * @param packageName the package containing a JAXB Object Factory
     * @return a JAXB Object factory
     * @throws CobolBindingException if JAXB classes are not found
     */
    public static Object newJaxbObjectFactory(final String packageName)
            throws CobolBindingException {

        try {
            return ClassUtil.newObject(packageName, JAXB_OBJECTFACTORY_NAME);
        } catch (ClassLoadingException e) {
            throw new CobolBindingException(e);
        }
    }

    /**
     * Loads a JAXB object using a JAXB Object factory.
     * 
     * @param jaxbObjectFactory the JAXB Object factory
     * @param jaxbClassName the JAXB class name
     * @return a new instance of the JAXB object
     * @throws CobolBindingException if instantiation failed
     */
    public static Object newJaxbObject(final Object jaxbObjectFactory,
            final String jaxbClassName) throws CobolBindingException {
        try {
            Method creator = ClassUtil.getCreatorMethod(jaxbObjectFactory,
                    jaxbClassName);
            return creator.invoke(jaxbObjectFactory);
        } catch (IllegalAccessException e) {
            throw new CobolBindingException(e);
        } catch (SecurityException e) {
            throw new CobolBindingException(e);
        } catch (IllegalArgumentException e) {
            throw new CobolBindingException(e);
        } catch (InvocationTargetException e) {
            throw new CobolBindingException(e);
        } catch (ClassMethodException e) {
            throw new CobolBindingException(e);
        }
    }

    /**
     * Create an instance of Transformers for a given JAXB root class name.
     * Assumes binding classes were generated for this JAXB class.
     * 
     * @param jaxbPackageName the JAXB package name
     * @param jaxbClassName the JAXB root class name
     * @return a new instance of Transformers
     * @throws CobolBindingException if transformers cannot be created
     */
    public static IHostTransformers newTransformers(
            final String jaxbPackageName, final String jaxbClassName)
            throws CobolBindingException {
        try {
            String coxbPackageName = (jaxbPackageName == null) ? COXB_PACKAGENAME_SUFFIX
                    : jaxbPackageName + "." + COXB_PACKAGENAME_SUFFIX;
            String transformersClassName = jaxbClassName + "Transformers";
            return (IHostTransformers) ClassUtil.newObject(coxbPackageName,
                    transformersClassName);
        } catch (ClassLoadingException e) {
            throw new CobolBindingException(e);
        }

    }

    /**
     * Create an instance of Transformers for a given JAXB root class name.
     * Assumes binding classes were generated for this JAXB class.
     * 
     * @param jaxbQualifiedClassName the JAXB class name
     * @return a new instance of Transformers
     * @throws CobolBindingException if transformers cannot be created
     */
    public static IHostTransformers newTransformers(
            final String jaxbQualifiedClassName) throws CobolBindingException {
        ClassName className = ClassUtil.toClassName(jaxbQualifiedClassName);
        return BindingUtil.newTransformers(className.packageName,
                className.className);
    }

    /**
     * Since JAXB classes may hide a POJO, this method gets a special
     * javaClassName annotation from the JAXB class. Such an annotation is
     * planted by schema generators and propagated by jaxbgen. If no such
     * annotation is present, then the JAXB class itself is returned as it is
     * considered it is not hiding a POJO.
     * 
     * @param jaxbPackage the JAXB package name
     * @param jaxbTypeName the JAXB type name
     * @return a class name (including package name) that the JAXB class is
     *         hiding or the JAXB class itself if it is not hiding a POJO.
     * @throws CobolBindingException if getting annotation fails
     */
    public static String getJavaClassName(final String jaxbPackage,
            final String jaxbTypeName) throws CobolBindingException {
        try {
            /* Load the JAXB class from the package */
            String qualifiedClassName = ClassUtil.toQualifiedClassName(
                    jaxbPackage, jaxbTypeName);
            Class < ? > clazz = ClassUtil.loadClass(qualifiedClassName);

            /* Get the complex type annotation if any */
            CobolComplexType annotation = clazz
                    .getAnnotation(CobolComplexType.class);
            if (annotation != null && annotation.javaClassName() != null
                    && annotation.javaClassName().length() > 0) {
                return annotation.javaClassName();
            }
            /* No annotations found, just return the JAXB class itself */
            return qualifiedClassName;

        } catch (ClassNotFoundException e) {
            throw (new CobolBindingException(e));
        }

    }

    /**
     * This method determines the relevant java type to be stored in a binding
     * element. In case of list items, we seek the items types rather than the
     * generic java.util.List type.
     * 
     * @param hostField field from which java type is extracted
     * @return the java type class
     * @throws CobolBindingException if class cannot be determined
     */
    public static Class < ? > getJavaClass(final Field hostField)
            throws CobolBindingException {
        Class < ? > javaClass = hostField.getType();
        if (javaClass.getName().compareTo("java.util.List") == 0) {
            ParameterizedType type = (ParameterizedType) hostField
                    .getGenericType();
            if (type.getActualTypeArguments().length != 1) {
                throw new CobolBindingException("Unsupported java class "
                        + javaClass.toString());
            }
            Type argType = type.getActualTypeArguments()[0];
            if (argType instanceof Class < ? >) {
                return (Class < ? >) argType;
            }
            return argType.getClass();
        }
        return javaClass;

    }

    /**
     * Returns the binding associated jaxb type name. Since certain bindings are
     * not associated with a jaxb property, this might return null.
     * 
     * @param binding the binding for which the jaxb type is to be returned
     * @return the binding associated jaxb type name
     */
    public static String getJaxbTypeName(final ICobolBinding binding) {
        if (binding.getJaxbType() == null) {
            return null;
        }
        return ClassUtil.toPropertyType(binding.getJaxbType().getName());
    }

    /**
     * Builds a binding type name using the associated jaxb type name.
     * 
     * @param binding the binding for which the binding type is to be returned
     * @return the binding type name
     */
    public static String getCoxbTypeName(final ICobolBinding binding) {
        /*
         * If there is no bound jaxb object, the name of the binding type is
         * built from the binding name
         */
        if (binding.getJaxbType() == null) {
            return NameUtil.upperFirstChar(binding.getBindingName())
                    + binding.getBindingName().substring(1,
                            binding.getBindingName().length()) + BIND_SUFFIX;
        }

        /* Arrays of complex bindings are returned as wrappers */
        if (binding instanceof ICobolArrayComplexBinding) {
            return getJaxbTypeName(binding) + WRAPPER_SUFFIX + BIND_SUFFIX;
        }
        return getJaxbTypeName(binding) + BIND_SUFFIX;
    }

    /**
     * Returns a name that can be used as a field name.
     * 
     * @param binding the binding for which the field name is to be returned
     * @return the binding proposed field name
     */
    public static String getFieldName(final ICobolBinding binding) {
        return NameUtil.lowerFirstChar(binding.getBindingName())
                + binding.getBindingName().substring(1,
                        binding.getBindingName().length());
    }

    /**
     * Retrieves the XML namespace associated with a JAXB element.
     * 
     * @param jaxbPackageName a JAXB element package name
     * @param jaxbTypeName a JAXB element type name
     * @return the XML namespace
     * @throws HostException if retrieving XML element name fails
     */
    public static String getXmlNamespace(final String jaxbPackageName,
            final String jaxbTypeName) throws HostException {
        try {
            JAXBElementDescriptor descriptor = new JAXBElementDescriptor(
                    jaxbPackageName, jaxbTypeName);
            return descriptor.getNamespace();
        } catch (JAXBAnnotationException e) {
            throw new HostException(e);
        }
    }

}
