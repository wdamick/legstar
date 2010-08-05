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
package com.legstar.coxb.gen;

import java.io.File;

import junit.framework.TestCase;


/**
 * Test extraction of JAXB object factory knowing the package name.
 *
 */
public class GetObjectFactoryTest extends TestCase {

    /** Generated JAXB classes binaries. */
    private static final String JAXB_DIR = "../legstar-jaxbgen-cases/target/classes";

    /** Attempt to get factory object from the wrong location should fail. */
    public void testInvalidLocation() {
        try {
            Object of = CoxbBindingGenerator.getObjectFactory(
                    "com.legstar.truc.coxb.alltypes",
                    new File("gen-bin"));
            fail("Invalid location test failed " + of.getClass().getName());
        } catch (Exception e) {
            assertEquals("ClassNotFoundException com.legstar.truc.coxb.alltypes.ObjectFactory in gen-bin",
                    e.getMessage());
        }
    }

    /** Location is correct but package does not exist. */
    public void testInvalidPackage() {
        try {
            CoxbBindingGenerator.getObjectFactory(
                    "com.legstar.test.truc.ALLTYPES",
                    new File(JAXB_DIR));
            fail("Invalid package test failed");
        } catch (Exception e) {
            assertEquals("ClassNotFoundException com.legstar.test.truc.ALLTYPES.ObjectFactory in"
                    + " ..\\legstar-jaxbgen-cases\\target\\classes", e.getMessage());
        }
    }

    /** Should succeed since both package and location are correct. */
    public void testGetObjectFactory() {
        Object of = CoxbBindingGenerator.getObjectFactory(
                "com.legstar.test.coxb.alltypes",
                new File(JAXB_DIR));
        assertEquals("com.legstar.test.coxb.alltypes.ObjectFactory", of.getClass().getName());
    }

}
