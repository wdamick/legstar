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

import com.legstar.codegen.CodeGenUtil;

/**
 * Test handling of enumeration types.
 * 
 */
public class EnumgenTest extends AbstractTestTemplate {

    /** @{inheritDoc  */
    public void setUp() throws Exception {
        super.setUp();
        CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
    }

    /**
     * Check for a successful generation.
     * 
     * @throws Exception if generation fails
     */
    public void testGenEnumTypes() throws Exception {
        CoxbBindingGenerator gen = new CoxbBindingGenerator();
        gen.setJaxbBinDir(JAXB_BIN_DIR);
        gen.setJaxbPackageName("com.legstar.test.coxb.enumvar");
        gen.setJaxbRootClassName("SearchRequestType");
        gen.setTargetDir(GEN_SRC_DIR);
        gen.execute();
        String srce = getSource(GEN_SRC_DIR,
                "com/legstar/test/coxb/enumvar/bind/SearchRequestTypeBinding.java");
        assertTrue(srce.contains("package com.legstar.test.coxb.enumvar.bind"));
    }

}
