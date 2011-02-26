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

import java.io.File;

/**
 * Test extraction of package name from annotated XSD.
 * 
 */
public class GetPackageNameTest extends AbstractCoxbGenTest {

    /** An instance of the generator model. */
    private CoxbGenModel _coxbGenModel;

    /**
     * (non-Javadoc).
     * 
     * @see junit.framework.TestCase#setUp()
     */
    public void setUp() {
        _coxbGenModel = new CoxbGenModel();
    }

    /** Test a non existing file. */
    public void testInvalidFile() {

        try {
            _coxbGenModel.getJaxbPackageNameFromXsd(new File("toto"));
            fail("invalid file test failed");
        } catch (Exception e) {
            assertEquals("IOException", e.getMessage().substring(0, 11));
        }

    }

    /** Test a non XML file. */
    public void testNotXML() {

        try {
            _coxbGenModel.getJaxbPackageNameFromXsd(new File(
                    "src/test/resources/notxml.xml"));
            fail("Not XML test failed");
        } catch (Exception e) {
            assertEquals("SAXException Content is not allowed in prolog.", e
                    .getMessage());
        }

    }

    /** Test an XML file with no target namespace. */
    public void testNoTargetNamespace() {

        try {
            assertEquals(
                    "generated",
                    _coxbGenModel
                            .getJaxbPackageNameFromXsd(new File(
                                    "src/test/resources/XSDWithoutTargetNamespace.xsd")));
        } catch (Exception e) {
            fail(e.getMessage());
        }

    }

    /** Test a valid XML file with JAXB annotation. */
    public void testWithJAXBAnnotations() {

        try {
            String pkg = _coxbGenModel.getJaxbPackageNameFromXsd(new File(
                    "../legstar-jaxbgen/" + COB_XSD_DIR, "ALLTYPES.xsd"));
            assertEquals("com.legstar.test.coxb.alltypes", pkg);
        } catch (Exception e) {
            fail("No annotations found " + e.getMessage());
        }

    }
}
