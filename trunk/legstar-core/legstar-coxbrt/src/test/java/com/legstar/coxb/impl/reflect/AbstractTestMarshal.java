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


import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * Generic methods shared by all marshaling tests 5java to Host).
 *
 */
public class AbstractTestMarshal extends TestCase {
    
    /** An implementation of COBOL to Java converters. */
    private ICobolConverters mConverters;
    
    /** {@inheritDoc} */
    public void setUp() {
        mConverters = new CobolSimpleConverters(new CobolContext());
    }

    /**
     * A generic method to convert from java to host (marshaling).
     * @param objectFactory the JAXB object factory
     * @param valueObject the input JAXB value object
     * @param expectedValue the hex string representing host result data
     */
    public void convertAndCheck(
            final Object objectFactory,
            final Object valueObject,
            final String expectedValue) {
        try {
            byte[] hostBytes = new byte[expectedValue.length() / 2];
            CobolMarshalVisitor mv = new CobolMarshalVisitor(
                    hostBytes, 0, getConverters());

            // Traverse the object structure, visiting each node with the visitor
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    objectFactory, valueObject);
            ccem.accept(mv);
            assertEquals(expectedValue, HostData.toHexString(hostBytes));
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostException e) {
            fail(e.getMessage());
        }
    }

    /**
     * @return the implementation of COBOL to Java converters
     */
    public ICobolConverters getConverters() {
        return mConverters;
    }
    
    

}
