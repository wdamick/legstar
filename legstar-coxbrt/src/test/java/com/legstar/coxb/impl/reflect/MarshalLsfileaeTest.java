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
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.lsfileae.ObjectFactory;

/**
 * Test LSFILEAE.
 *
 */
public class MarshalLsfileaeTest extends TestCase {

    /**
     * Test Marshaling.
     * @throws HostException if marshaling fails
     */
    public void testLsfileae() throws HostException{
        CobolContext cobolContext = new CobolContext();
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        byte[] hostBytes = new byte[LsfileaeCases.getHostBytesHex().length() / 2];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
        ObjectFactory objectFactory = new ObjectFactory();
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                objectFactory, LsfileaeCases.getJavaObject());
        ccem.accept(mv);
        assertEquals(LsfileaeCases.getHostBytesHex(),HostData.toHexString(hostBytes));
    }

    /**
     * Test Transforming.
     */
    public void testTransformLsfileae() {
        try {
            ReflectJavaToHostTransformer transformer =
                new ReflectJavaToHostTransformer(
                        "com.legstar.test.coxb.lsfileae", "Dfhcommarea");
            byte[] hostBytes = transformer.transform(LsfileaeCases.getJavaObject());
            assertEquals(LsfileaeCases.getHostBytesHex(),
                    HostData.toHexString(hostBytes));
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}
