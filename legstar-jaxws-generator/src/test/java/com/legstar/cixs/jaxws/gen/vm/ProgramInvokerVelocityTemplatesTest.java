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
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation of a an operation invoker class for Adapters.
 */
public class ProgramInvokerVelocityTemplatesTest extends AbstractTestTemplate {

    /**
     * Case of a commarea driven target program.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {

        CixsJaxwsService service = Samples.getLsfileae();
        CixsOperation operation = service.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateProgramInvoker(operation, getParameters(),
                operationClassFilesDir);
        check(service, operation);
    }

    /**
     * Case where input commarea is different from output commarea.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileal() throws Exception {

        CixsJaxwsService service = Samples.getLsfileal();
        CixsOperation operation = service.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateProgramInvoker(operation, getParameters(),
                operationClassFilesDir);
        check(service, operation);
    }

    /**
     * Case where input commarea is different from output commarea.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileac() throws Exception {

        CixsJaxwsService service = Samples.getLsfileac();
        CixsOperation operation = service.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateProgramInvoker(operation, getParameters(),
                operationClassFilesDir);
        check(service, operation);
    }

    /**
     * Case where the operation has a different namespace/package than the
     * service.
     * 
     * @throws Exception if test fails
     */
    public void testLsfilean() throws Exception {

        CixsJaxwsService service = Samples.getLsfilean();
        CixsOperation operation = service.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateProgramInvoker(operation, getParameters(),
                operationClassFilesDir);
        check(service, operation);
    }

    /**
     * Case where there is no package name.
     * 
     * @throws Exception if test fails
     */
    public void testLsfileap() throws Exception {

        CixsJaxwsService jaxwsService = Samples.getLsfileap();
        CixsOperation operation = jaxwsService.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateProgramInvoker(operation, getParameters(),
                operationClassFilesDir);
        String resStr = getSource(operationClassFilesDir,
                operation.getClassName() + "ProgramInvoker.java");

        assertFalse(resStr.contains("package "));
    }

    /**
     * Check generated artifact against the reference.
     * 
     * @param operation the model's operation
     * @throws Exception if can't get reference
     */
    protected void check(final CixsJaxwsService model,
            final CixsOperation operation) throws Exception {
        String fileName = operation.getPackageName().replace(".", "/") + "/"
                + operation.getClassName() + "ProgramInvoker.java";
        check(new File(REF_SRC_DIR, fileName), new File(GEN_SRC_DIR, fileName));
    }
}
