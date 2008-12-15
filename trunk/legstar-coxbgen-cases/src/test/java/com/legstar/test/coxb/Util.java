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

public class Util {

    public static Object getJaxbObject(String schemaName) throws Exception {
        return getJaxbObject(schemaName, "Dfhcommarea");
    }

    @SuppressWarnings("unchecked")
    public static Object getJaxbObject(String schemaName, String jaxbTypeName) throws Exception {
        // Create a JAXB object factory
        String ofClassName = "com.legstar.test.coxb." + schemaName + ".ObjectFactory";
        Class ofClass = JaxbUtil.loadClass(ofClassName);
        Object of = ofClass.newInstance();

        // Create a JAXB object
        Method create = ofClass.getMethod("create" + jaxbTypeName);
        Object jaxbObject = create.invoke(of);
        return jaxbObject;
    }

    @SuppressWarnings("unchecked")
    public static Object getBindingObject(String schemaName, String jaxbTypeName) throws Exception {
        return getBindingObject(schemaName, jaxbTypeName, null);
    }

    @SuppressWarnings("unchecked")
    public static Object getBindingObject(String schemaName, String jaxbTypeName, Object jaxbObject) throws Exception {
        // Create a complex binding
        String bindClassName = "com.legstar.test.coxb." + schemaName + ".bind." + jaxbTypeName + "Binding";
        Class bindClass = JaxbUtil.loadClass(bindClassName);
        Constructor constructor;
        if (jaxbObject == null)	{
            constructor = bindClass.getConstructor();
            return constructor.newInstance();
        } else {
            constructor = bindClass.getConstructor(JaxbUtil.loadClass("com.legstar.test.coxb." + schemaName + "." + jaxbTypeName));
            return constructor.newInstance(jaxbObject);
        }
    }

    public static Object marshal(String schemaName, Object jaxbObject, int byteLength) throws Exception {
        return marshal(schemaName, "Dfhcommarea", jaxbObject, byteLength);
    }

    public static String marshal(String schemaName, String jaxbTypeName, Object jaxbObject, int byteLength) throws Exception {
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

    public static Object unmarshal(byte[] hostBytes, String schemaName) throws Exception {
        return unmarshal(hostBytes, schemaName, "Dfhcommarea");
    }

    @SuppressWarnings("unchecked")
    public static Object unmarshal(byte[] hostBytes, String schemaName, String jaxbTypeName) throws Exception {
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

    public static int getByteLength(String schemaName) throws Exception {
        return getByteLength(schemaName, "Dfhcommarea");
    }

    @SuppressWarnings("unchecked")
    public static int getByteLength(String schemaName, String jaxbTypeName) throws Exception {
        Object bind = getBindingObject(schemaName, jaxbTypeName);
        return ((ICobolBinding) bind).calcByteLength();
    }
}
