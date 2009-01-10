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
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.lsfileae.ObjectFactory;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

/**
 * Test LSFILEAE.
 *
 */
public class UnmarshalLsfileaeTest extends TestCase {

    /**
     * Unmarshal Lsfileae.
     * @throws HostException if anything goes wrong
     * @throws ClassNotFoundException 
     */
    public final void testLsfileae() throws HostException, ClassNotFoundException {
        CobolContext cobolContext = new CobolContext();
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        byte[] hostBytes = HostData.toByteArray(LsfileaeCases.getHostBytesHex());
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
        ObjectFactory objectFactory = new ObjectFactory();
        CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
                Dfhcommarea.class);
        ccem.accept(uv);
        Dfhcommarea dfhcommarea = (Dfhcommarea) ccem.getObjectValue(Dfhcommarea.class);
        LsfileaeCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Test Transforming.
     */
    public void testTransformLsfileae() {
        try {
            ReflectHostToJavaTransformer transformer =
                new ReflectHostToJavaTransformer(
                        "com.legstar.test.coxb.lsfileae", "Dfhcommarea");
            Dfhcommarea dfhcommarea = transformer.transform(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHex()));
            LsfileaeCases.checkJavaObject(dfhcommarea);
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}
