/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.util.BindingUtil;
import com.legstar.coxb.util.ClassUtil;

/**
 * A helper class for testing cases.
 * 
 */
public final class Util {

    /** Utility class. */
    private Util() {

    }

    /**
     * Return the JAXB object when they are called dfhcommarea.
     * 
     * @param schemaName the XSD annotated name
     * @return a JAXB object
     * @throws Exception if JAXB object cannot be recovered
     */
    public static Object getJaxbObject(final String schemaName)
            throws Exception {
        return getJaxbObject(schemaName, "Dfhcommarea");
    }

    /**
     * Return the JAXB object.
     * 
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @return a JAXB object
     * @throws Exception if JAXB object cannot be recovered
     */
    public static Object getJaxbObject(final String schemaName,
            final String jaxbTypeName) throws Exception {
        // Create a JAXB object factory
        String jaxbPackageName = "com.legstar.test.coxb." + schemaName;
        Object of = BindingUtil.newJaxbObjectFactory(jaxbPackageName);

        // Create a JAXB object
        return BindingUtil.newJaxbObject(of, jaxbTypeName);
    }

    /**
     * Return the Binding object.
     * 
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @return a Binding object
     * @throws Exception if Binding object cannot be recovered
     */
    public static Object getBindingObject(final String schemaName,
            final String jaxbTypeName) throws Exception {
        return getBindingObject(schemaName, jaxbTypeName, null);
    }

    /**
     * Return the Binding object, bound to an object.
     * 
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @param jaxbObject the JAXB object
     * @return a Binding object
     * @throws Exception if Binding object cannot be recovered
     */
    public static Object getBindingObject(
            final String schemaName,
            final String jaxbTypeName,
            final Object jaxbObject) throws Exception {
        // Create a complex binding
        String bindClassName = "com.legstar.test.coxb." + schemaName + ".bind."
                + jaxbTypeName + "Binding";
        Class < ? > bindClass = ClassUtil.loadClass(bindClassName);
        Constructor < ? > constructor;
        if (jaxbObject == null) {
            constructor = bindClass.getConstructor();
            return constructor.newInstance();
        } else {
            constructor = bindClass
                    .getConstructor(ClassUtil.loadClass(
                            "com.legstar.test.coxb." + schemaName + "."
                                    + jaxbTypeName));
            return constructor.newInstance(jaxbObject);
        }
    }

    /**
     * Marshal a java data object to host data.
     * 
     * @param schemaName the XSD annotated name
     * @param jaxbObject the JAXB object
     * @param byteLength expected byte length
     * @return a String representing host byte data in hex
     * @throws Exception if marshaling fails
     */
    public static String marshal(
            final String schemaName,
            final Object jaxbObject,
            final int byteLength) throws Exception {
        return marshal(schemaName, "Dfhcommarea", jaxbObject, byteLength);
    }

    /**
     * Marshal a java data object to host data.
     * 
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @param jaxbObject the JAXB object
     * @param byteLength expected byte length
     * @return a String representing host byte data in hex
     * @throws Exception if marshaling fails
     */
    public static String marshal(
            final String schemaName,
            final String jaxbTypeName,
            final Object jaxbObject,
            final int byteLength) throws Exception {
        byte[] hostBytes = new byte[byteLength];
        // Create a cobol context
        CobolContext cobolContext = new CobolContext();
        // Select a conversion strategy
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        // Create a concrete visitor
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
        // Create a binding instance
        Object bind = getBindingObject(schemaName, jaxbTypeName, jaxbObject);

        // Traverse the object structure, visiting each node with the visitor
        Method accept = bind.getClass().getMethod("accept",
                CobolElementVisitor.class);
        accept.invoke(bind, mv);
        return HostData.toHexString(hostBytes);
    }

    /**
     * Unmarshal host data into a java data object.
     * 
     * @param hostBytes the host data
     * @param schemaName the XSD annotated name
     * @return a java data object
     * @throws Exception if unmarshaling fails
     */
    public static Object unmarshal(
            final byte[] hostBytes,
            final String schemaName) throws Exception {
        return unmarshal(hostBytes, schemaName, "Dfhcommarea");
    }

    /**
     * Unmarshal host data into a java data object.
     * 
     * @param hostBytes the host data
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @return a java data object
     * @throws Exception if unmarshaling fails
     */
    public static Object unmarshal(
            final byte[] hostBytes,
            final String schemaName,
            final String jaxbTypeName) throws Exception {
        // Create a cobol context
        CobolContext cobolContext = new CobolContext();
        // Select a conversion strategy
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        // Create a concrete visitor
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
        // Create a binding instance
        Object bind = getBindingObject(schemaName, jaxbTypeName);

        Method accept = bind.getClass().getMethod("accept",
                CobolElementVisitor.class);
        accept.invoke(bind, uv);

        Method getDfhcommarea = bind.getClass().getMethod("get" + jaxbTypeName,
                (Class[]) null);
        Object jaxbObject = getDfhcommarea.invoke(bind);
        return jaxbObject;
    }

    /**
     * Calculates the max host bytes size.
     * 
     * @param schemaName the XSD annotated name
     * @return the max host bytes size
     * @throws Exception if calculation fails
     */
    public static int getByteLength(final String schemaName) throws Exception {
        return getByteLength(schemaName, "Dfhcommarea");
    }

    /**
     * Calculates the max host bytes size.
     * 
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @return the max host bytes size
     * @throws Exception if calculation fails
     */
    public static int getByteLength(
            final String schemaName,
            final String jaxbTypeName) throws Exception {
        Object bind = getBindingObject(schemaName, jaxbTypeName);
        return ((ICobolBinding) bind).getByteLength();
    }
}
