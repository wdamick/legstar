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
package com.legstar.coxb.impl.visitor;

import junit.framework.TestCase;

import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.test.coxb.coxb137.BoolPojo;

/**
 * There are several marshaling tests elsewhere in this project. Here we focus
 * on special cases.
 * 
 */
public class CobolMarshalVisitorTest extends TestCase {

    /**
     * Tests related to http://code.google.com/p/legstar/issues/detail?id=137.
     * 
     * @throws Exception if unmarshaling fails
     */
    public void testBooleanField() throws Exception {
        BoolPojo boolPojo = new BoolPojo();
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.coxb137.ObjectFactory(), boolPojo);

        byte[] hostBytes = new byte[6];
        boolPojo.setABoolean(true);
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals("000000000001", HostData.toHexString(hostBytes));

        hostBytes = new byte[8];
        boolPojo.setABoolean(true);
        boolPojo.getBooleanList().add(false);
        mv = new CobolMarshalVisitor(hostBytes, 0, new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals("0000000100000001", HostData.toHexString(hostBytes));

    }
}
