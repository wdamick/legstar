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

import com.legstar.coxb.CobolBindingException;

import junit.framework.TestCase;

/**
 * Test the BindingUtil class.
 * 
 */
public class BindingUtilTest extends TestCase {

    /**
     * Test ability to create new JAXB object factories dynamically.
     */
    public void testNewJaxbObjectFactory() {
        try {
            Object factory = BindingUtil
                    .newJaxbObjectFactory("com.legstar.test.coxb.mock");
            assertTrue(factory != null);
            assertTrue(factory instanceof com.legstar.test.coxb.mock.ObjectFactory);
        } catch (CobolBindingException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    /**
     * Test ability to create new JAXB object dynamically.
     */
    public void testNewJaxbObject() {
        try {
            Object object = BindingUtil.newJaxbObject(
                    new com.legstar.test.coxb.mock.ObjectFactory(),
                    "Dfhcommarea");
            assertTrue(object != null);
            assertTrue(object instanceof com.legstar.test.coxb.mock.Dfhcommarea);
        } catch (CobolBindingException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    /**
     * Test ability to create new Transformers dynamically.
     */
    public void testnewTransformers() {
        try {
            Object transformers = BindingUtil.newTransformers(
                    "com.legstar.test.coxb.mock", "Dfhcommarea");
            assertTrue(transformers != null);
            assertTrue(transformers instanceof com.legstar.test.coxb.mock.bind.DfhcommareaTransformers);
        } catch (CobolBindingException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }
}
