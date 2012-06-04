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
package com.legstar.cixs.jaxws.gen.vm;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation of a JAX-WS endpoint implementation for an adapter.
 * 
 */
public class ImplementationVelocityTemplatesTest extends AbstractTestTemplate {

    /**
     * Case of a commarea driven target program.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {

        CixsJaxwsService model = Samples.getLsfileae();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case of a commarea driven target program with input layout different from
     * output layout.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileal() throws Exception {

        CixsJaxwsService model = Samples.getLsfileal();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case of a container driven target program.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileac() throws Exception {

        CixsJaxwsService model = Samples.getLsfileac();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case of a multi method service adapter.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileax() throws Exception {

        CixsJaxwsService model = Samples.getLsfileax();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case where the operation has a different package than the service.
     * 
     * @throws Exception if test fails
     */
    public void testLsfilean() throws Exception {

        CixsJaxwsService model = Samples.getLsfilean();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case where there is no package name.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileap() throws Exception {

        CixsJaxwsService model = Samples.getLsfileap();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(model, getParameters(),
                componentClassFilesDir);
        String resStr = getSource(componentClassFilesDir,
                model.getImplementationClassName() + ".java");

        assertFalse(resStr.contains("package com.legstar.test.cixs.lsfileap;"));
        assertTrue(resStr
                .contains("import com.legstar.test.coxb.lsfileae.Dfhcommarea;"));
        assertTrue(resStr
                .contains("private static final String  SERVICE_ADAPTER_NAME = \"lsfileap\";"));
        assertTrue(resStr
                .contains("private LsfileaeProgramInvoker mLsfileaeProgramInvoker;"));
        assertTrue(resStr
                .contains("mLsfileaeProgramInvoker = new LsfileaeProgramInvoker(getConfigFileName());"));
        assertTrue(resStr.contains("public Dfhcommarea lsfileae("));
        assertTrue(resStr.contains("final Dfhcommarea request,"));
    }

    /**
     * Check generated artifact against the reference.
     * 
     * @param service the generation model
     * @throws Exception if can't get reference
     */
    protected void check(final CixsJaxwsService service) throws Exception {
        String fileName = GEN_SRC_SUBDIR + service.getName() + "/"
                + service.getImplementationClassName() + ".java";
        check(new File(REF_SRC_DIR, fileName), new File(GEN_SRC_DIR, fileName));
    }
}
