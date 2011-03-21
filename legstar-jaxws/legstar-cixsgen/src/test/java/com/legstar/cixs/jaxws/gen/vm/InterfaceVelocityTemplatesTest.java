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
 * Test the generation of a JAX-WS endpoint interface for an adapter.
 * 
 */
public class InterfaceVelocityTemplatesTest extends AbstractTestTemplate {

    /**
     * Generate the package name.
     * 
     * @throws Exception if test fails
     */
    public void testCommonPackage() throws Exception {

        CixsJaxwsService model = Samples.getLsfileae();
        String resStr = genSource(model,
                Jaxws2CixsGenerator.JAXWS_TO_CIXS_GENERATOR_NAME,
                "vlc/j2c-service-common-package.vm", GEN_SRC_DIR, "test.txt");
        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
    }

    /**
     * Case of a commarea driven target program.
     * 
     * @throws Exception if test fails
     */
    public void testInterfaceLsfileae() throws Exception {

        CixsJaxwsService model = Samples.getLsfileae();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateInterface(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case of a commarea driven target program with input layout different from
     * output layout.
     * 
     * @throws Exception if test fails
     */
    public void testInterfaceLsfileal() throws Exception {

        CixsJaxwsService model = Samples.getLsfileal();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateInterface(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case of a container driven target program.
     * 
     * @throws Exception if test fails
     */
    public void testInterfaceLsfileac() throws Exception {

        CixsJaxwsService model = Samples.getLsfileac();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateInterface(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case of a a multiple method service.
     * 
     * @throws Exception if test fails
     */
    public void testInterfaceLsfileax() throws Exception {

        CixsJaxwsService model = Samples.getLsfileax();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateInterface(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case where operation has a different package than the service.
     * 
     * @throws Exception if test fails
     */
    public void testInterfaceLsfilean() throws Exception {

        CixsJaxwsService model = Samples.getLsfilean();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateInterface(model, getParameters(),
                componentClassFilesDir);
        check(model);
    }

    /**
     * Case where where there is no operation package names (not even one
     * inherited from the service).
     * 
     * @throws Exception if test fails
     */
    public void testInterfaceLsfileap() throws Exception {

        CixsJaxwsService model = Samples.getLsfileap();
        initWebServiceParameters(model);

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateInterface(model, getParameters(),
                componentClassFilesDir);
        assertTrue(new File(GEN_SRC_DIR, "Lsfileap.java").exists());
    }

    /**
     * Check generated artifact against the reference.
     * 
     * @param service the generation model
     * @throws Exception if can't get reference
     */
    protected void check(final CixsJaxwsService service) throws Exception {
        String fileName = GEN_SRC_SUBDIR + service.getName() + "/"
                + service.getInterfaceClassName() + ".java";
        check(new File(REF_SRC_DIR, fileName), new File(GEN_SRC_DIR, fileName));
    }
}
