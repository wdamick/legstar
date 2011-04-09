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
package com.legstar.coxb.impl.reflect;

import junit.framework.TestCase;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;

/**
 * Generic methods shared by all unmarshaling tests (Host to Java).
 * 
 */
public class AbstractTestUnmarshal extends TestCase {

    /** An implementation of COBOL to Java converters. */
    private ICobolConverters mConverters;

    /** {@inheritDoc} */
    public void setUp() {
        mConverters = new CobolSimpleConverters(new CobolContext());
    }

    /**
     * A generic method to convert from host to java (unmarshaling).
     * 
     * @param objectFactory the JAXB object factory
     * @param hostBytesHex the input hex string representing host data
     * @param expectedValue the expected JAXB value object
     * @return the JAXB value object
     */
    public Object convert(final Object objectFactory,
            final String hostBytesHex, final Object expectedValue) {
        try {
            byte[] hostBytes = HostData.toByteArray(hostBytesHex);
            CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                    getConverters());

            // Traverse the object structure, visiting each node with the
            // visitor
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    objectFactory, expectedValue.getClass());
            ccem.accept(uv);
            return ccem.getObjectValue(expectedValue.getClass());
        } catch (ReflectBindingException e) {
            e.printStackTrace();
            fail(e.getMessage());
        } catch (HostException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
        return null;
    }

    /**
     * @return the implementation of COBOL to Java converters
     */
    public ICobolConverters getConverters() {
        return mConverters;
    }

}
