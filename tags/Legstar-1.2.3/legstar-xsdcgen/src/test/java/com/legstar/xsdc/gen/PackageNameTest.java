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
package com.legstar.xsdc.gen;

/**
 * Check package name handling.
 *
 */
public class PackageNameTest extends AbstractTest {

    /**
     * Target name is mixed case.
     * @throws Exception if generation fails
     */
    public void testMixedCaseName() throws Exception {
        XsdCobolAnnotator xsdCobolAnnotator = createXsdCobolAnnotator();
        xsdCobolAnnotator.setInputXsdUri(getSchemaFileURI("singleSimpleElement.xsd"));
        xsdCobolAnnotator.execute();
        /* Read the resulting output source*/
        String result = getSource(GEN_DIR, "singleSimpleElement.xsd");
        assertTrue(result.contains("<jaxb:package name=\"com.example.finance.creditcardfaults.xsd\"/>"));
    }

}
