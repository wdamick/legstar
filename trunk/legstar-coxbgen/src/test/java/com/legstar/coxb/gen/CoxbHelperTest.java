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
package com.legstar.coxb.gen;

import junit.framework.TestCase;

import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.coxb.util.ClassUtil;

/**
 * Test the CoxbHelper.
 * 
 */
public class CoxbHelperTest extends TestCase {

    /** Cocb helper instance. */
    private CoxbHelper _coxbHelper = new CoxbHelper();

    /**
     * Get the bound type from a binding class.
     * 
     * @throws Exception if bound type cannot be extracted
     */
    public void testGetBoundTypeName() throws Exception {
        com.legstar.test.coxb.lsfilead.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfilead.ObjectFactory();

        CComplexReflectBinding binding = new CComplexReflectBinding(
                objectFactory,
                ClassUtil
                        .loadClass("com.legstar.test.coxb.alltypes.Dfhcommarea"));

        assertEquals("Dfhcommarea", _coxbHelper.getBoundTypeName(binding));

    }

    /**
     * Get the XML related annotations from a JAXB bound object.
     */
    public void testGetXmlAnnotations() {
        try {
            com.legstar.test.coxb.lsfilead.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfilead.ObjectFactory();

            CComplexReflectBinding binding = new CComplexReflectBinding(
                    objectFactory,
                    ClassUtil
                            .loadClass("com.legstar.test.coxb.alltypes.Dfhcommarea"));

            assertEquals("Dfhcommarea", _coxbHelper.getJaxbTypeName(binding));
            assertEquals("com.legstar.test.coxb.alltypes",
                    _coxbHelper.getJaxbPackageName(binding));
            assertEquals("Dfhcommarea", _coxbHelper.getXmlElementName(binding));
            assertEquals("http://legstar.com/test/coxb/alltypes",
                    _coxbHelper.getXmlNamespace(binding));
            assertEquals(false, _coxbHelper.isXmlRootElement(binding));
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (ClassNotFoundException e) {
            fail(e.getMessage());
        } catch (HostException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Test capability to identify JAXB indexed arrays as opposed to List.
     * 
     * @throws Exception if test fails
     */
    public void testIsJaxbArray() throws Exception {

        /* Test a simple List array */
        CComplexReflectBinding parent = new CComplexReflectBinding(
                new com.legstar.test.coxb.listssdo.ObjectFactory(),
                ClassUtil
                        .loadClass("com.legstar.test.coxb.listssdo.Dfhcommarea"));
        assertEquals(false, _coxbHelper.isJaxbArray(parent, parent
                .getChildrenList().get(1)));

        /* Test a complex List array */
        parent = new CComplexReflectBinding(
                new com.legstar.test.coxb.arrayscx.ObjectFactory(),
                ClassUtil
                        .loadClass("com.legstar.test.coxb.arrayscx.Dfhcommarea"));
        assertEquals(false, _coxbHelper.isJaxbArray(parent, parent
                .getChildrenList().get(1)));

        /* Test a complex indexed array */
        parent = new CComplexReflectBinding(
                new com.legstar.test.coxb.rq071.ObjectFactory(),
                ClassUtil.loadClass("com.legstar.test.coxb.rq071.RQ071Output"));
        assertEquals(true, _coxbHelper.isJaxbArray(parent, parent
                .getChildrenList().get(17)));
    }

}
