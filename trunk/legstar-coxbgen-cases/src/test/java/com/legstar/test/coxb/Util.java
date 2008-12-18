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
package com.legstar.test.coxb;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.util.JaxbUtil;

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
     * @param schemaName the XSD annotated name
     * @return a JAXB object
     * @throws Exception if JAXB object cannot be recovered
     */
    public static Object getJaxbObject(final String schemaName) throws Exception {
        return getJaxbObject(schemaName, "Dfhcommarea");
    }

    /**
     * Return the JAXB object.
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @return a JAXB object
     * @throws Exception if JAXB object cannot be recovered
     */
    public static Object getJaxbObject(final String schemaName,
            final String jaxbTypeName) throws Exception {
        // Create a JAXB object factory
        String ofClassName = "com.legstar.test.coxb." + schemaName + ".ObjectFactory";
        Class < ? > ofClass = JaxbUtil.loadClass(ofClassName);
        Object of = ofClass.newInstance();

        // Create a JAXB object
        Method create = ofClass.getMethod("create" + jaxbTypeName);
        Object jaxbObject = create.invoke(of);
        return jaxbObject;
    }

    /**
     * Return the Binding object.
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @return a Binding object
     * @throws Exception if Binding object cannot be recovered
     */
    public static Object getBindingObject(final String schemaName, final String jaxbTypeName) throws Exception {
        return getBindingObject(schemaName, jaxbTypeName, null);
    }

    /**
     * Return the Binding object, bound to an object.
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
        String bindClassName = "com.legstar.test.coxb." + schemaName + ".bind." + jaxbTypeName + "Binding";
        Class < ? > bindClass = JaxbUtil.loadClass(bindClassName);
        Constructor < ? > constructor;
        if (jaxbObject == null) {
            constructor = bindClass.getConstructor();
            return constructor.newInstance();
        } else {
            constructor = bindClass.getConstructor(JaxbUtil.loadClass(
                    "com.legstar.test.coxb." + schemaName + "." + jaxbTypeName));
            return constructor.newInstance(jaxbObject);
        }
    }

    /**
     * Marshal a java data object to host data.
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
        Method accept = bind.getClass().getMethod("accept", CobolElementVisitor.class);
        accept.invoke(bind, mv);
        return HostData.toHexString(hostBytes);
    }

    /**
     * Unmarshal host data into a java data object.
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

        Method accept = bind.getClass().getMethod("accept", CobolElementVisitor.class);
        accept.invoke(bind, uv);

        Method getDfhcommarea = bind.getClass().getMethod("get" + jaxbTypeName, (Class[]) null);
        Object jaxbObject = getDfhcommarea.invoke(bind);
        return jaxbObject;
    }

    /**
     * Calculates the max host bytes size.
     * @param schemaName the XSD annotated name
     * @return the max host bytes size
     * @throws Exception if calculation fails
     */
    public static int getByteLength(final String schemaName) throws Exception {
        return getByteLength(schemaName, "Dfhcommarea");
    }

    /**
     * Calculates the max host bytes size.
     * @param schemaName the XSD annotated name
     * @param jaxbTypeName the name of the JAXB class
     * @return the max host bytes size
     * @throws Exception if calculation fails
     */
    public static int getByteLength(
            final String schemaName,
            final String jaxbTypeName) throws Exception {
        Object bind = getBindingObject(schemaName, jaxbTypeName);
        return ((ICobolBinding) bind).calcByteLength();
    }
}
