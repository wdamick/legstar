/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.util;

import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * Test the general purpose JAXB utility methods.
 */
public class JaxbUtilTest extends TestCase {

    /**
     * Test extraction of the target POJO class name from annotations.
     */
    public void testGetJavaClassNameAnnotations() {
        try {
            assertEquals("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply",
                    JaxbUtil.getJavaClassName(
                            "com.legstar.test.coxb.jvmquery", "JvmQueryReply"));
        } catch (HostException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Case where there is no target POJO annotation.
     */
    public void testGetJavaClassNameNoAnnotations() {
        try {
            assertEquals("com.legstar.test.coxb.dplarcht.Dfhcommarea",
                    JaxbUtil.getJavaClassName(
                            "com.legstar.test.coxb.dplarcht", "Dfhcommarea"));
        } catch (HostException e) {
            fail(e.getMessage());
        }

    }
}
