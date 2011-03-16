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
 * Test the generation of a host program bean class for Adapters.
 */
public class HostProgramVelocityTemplatesTest extends AbstractTestTemplate {

    /**
     * Case of a commarea driven target program.
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {

        CixsJaxwsService jaxwsService = Samples.getLsfileae();
        CixsOperation operation = jaxwsService.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateHostProgram(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "HostProgram.java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("public class LsfileaeHostProgram extends HostProgram {"));
        assertTrue(resStr.contains("setName(\"LSFILEAE\");"));
        assertTrue(resStr.contains("setLength(79);"));
        assertTrue(resStr.contains("setDataLength(79);"));
    }

    /**
     * Case where input commarea is different from output commarea.
     * @throws Exception if test fails
     */
    public void testLsfileal() throws Exception {

        CixsJaxwsService jaxwsService = Samples.getLsfileal();
        CixsOperation operation = jaxwsService.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateHostProgram(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "HostProgram.java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileal;"));
        assertTrue(resStr.contains("public class LsfilealHostProgram extends HostProgram {"));
        assertTrue(resStr.contains("public LsfilealHostProgram() {"));
        assertTrue(resStr.contains("setName(\"LSFILEAL\");"));
        assertTrue(resStr.contains("setLength(8043);"));
        assertTrue(resStr.contains("setDataLength(20);"));
    }

    /**
     * Case where input commarea is different from output commarea.
     * @throws Exception if test fails
     */
    public void testLsfileac() throws Exception {

        CixsJaxwsService jaxwsService = Samples.getLsfileac();
        CixsOperation operation = jaxwsService.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateHostProgram(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "HostProgram.java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileac;"));
        assertTrue(resStr.contains("import com.legstar.host.invoke.model.HostContainer;"));
        assertTrue(resStr.contains("public class LsfileacHostProgram extends HostProgram {"));
        assertTrue(resStr.contains("public LsfileacHostProgram() {"));
        assertTrue(resStr.contains("setName(\"LSFILEAC\");"));
        assertTrue(resStr.contains("setChannel(\"LSFILEAC-CHANNEL\");"));
        assertTrue(resStr.contains("getInContainers().add(new HostContainer(\"QueryData\", 48));"));
        assertTrue(resStr.contains("getInContainers().add(new HostContainer(\"QueryLimit\", 10));"));
        assertTrue(resStr.contains("getOutContainers().add(new HostContainer(\"ReplyData\", 7905));"));
        assertTrue(resStr.contains("getOutContainers().add(new HostContainer(\"ReplyStatus\", 151));"));
    }

    /**
     * Case where the operation has a different namespace/package than the service.
     * @throws Exception if test fails
     */
    public void testLsfilean() throws Exception {

        CixsJaxwsService jaxwsService = Samples.getLsfilean();
        CixsOperation operation = jaxwsService.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateHostProgram(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "HostProgram.java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.oper.lsfilean;"));
    }

    /**
     * Case where there is no package name.
     * @throws Exception if test fails
     */
    public void testLsfileap() throws Exception {

        CixsJaxwsService jaxwsService = Samples.getLsfileap();
        CixsOperation operation = jaxwsService.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateHostProgram(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "HostProgram.java");

        assertFalse(resStr.contains("package "));
    }
}
