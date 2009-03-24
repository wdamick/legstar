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
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {

        CixsJaxwsService jaxwsService = Samples.getLsfileae();
        CixsOperation operation = jaxwsService.getCixsOperations().get(0);

        File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, operation.getPackageName(), true);
        Jaxws2CixsGenerator.generateProgramInvoker(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "ProgramInvoker.java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.Dfhcommarea;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTransformers;"));
        assertTrue(resStr.contains("* Implements an invoker for program LSFILEAE."));
        assertTrue(resStr.contains("* This host program maps to operation lsfileae."));
        assertTrue(resStr.contains("public class LsfileaeProgramInvoker extends AbstractProgramInvoker {"));
        assertTrue(resStr.contains("/** Host transformers for Dfhcommarea java data object. */"));
        assertTrue(resStr.contains("private DfhcommareaTransformers mDfhcommareaTransformers;"));
        assertTrue(resStr.contains("/** Operation name for lsfileae. */"));
        assertTrue(resStr.contains("private static final String  OPERATION_NAME = \"lsfileae\";"));
        assertTrue(resStr.contains("/** Properties for operation lsfileae. */"));
        assertTrue(resStr.contains("private static final String  PROGRAM_PROPERTIES = \"lsfileae.properties\";"));
        assertTrue(resStr.contains("public LsfileaeProgramInvoker(final String configFileName) {"));
        assertTrue(resStr.contains("mDfhcommareaTransformers = new DfhcommareaTransformers();"));
        assertTrue(resStr.contains("* Invoke the LSFILEAE host program."));
        assertTrue(resStr.contains("* @throws HostInvokerException if host invoker cannot be created or configured"));
        assertTrue(resStr.contains("public final Dfhcommarea lsfileae("));
        assertTrue(resStr.contains("final Dfhcommarea request)"));
        assertTrue(resStr.contains("throws HostInvokerException, HostTransformException {"));
        assertTrue(resStr.contains("getDfhcommareaTransformers().toHost(request, hostCharset);"));
        assertTrue(resStr.contains("byte[] replyBytes = hostInvoker.invoke(requestID, requestBytes);"));
        assertTrue(resStr.contains("return getDfhcommareaTransformers().toJava(replyBytes, hostCharset);"));
        
        assertTrue(resStr.contains("* @return the host transformers for Dfhcommarea"));
        assertTrue(resStr.contains("public DfhcommareaTransformers getDfhcommareaTransformers() {"));
        assertTrue(resStr.contains("return mDfhcommareaTransformers;"));
        assertTrue(resStr.contains("* @param transformers the host transformers for Dfhcommarea"));
        assertTrue(resStr.contains("public void setDfhcommareaTransformers("));
        assertTrue(resStr.contains("final DfhcommareaTransformers transformers) {"));
        assertTrue(resStr.contains("mDfhcommareaTransformers = transformers;"));
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
        Jaxws2CixsGenerator.generateProgramInvoker(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "ProgramInvoker.java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileal;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.RequestParms;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.ReplyData;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.bind.RequestParmsTransformers;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.bind.ReplyDataTransformers;"));
        assertTrue(resStr.contains("private RequestParmsTransformers mRequestParmsTransformers;"));
        assertTrue(resStr.contains("private ReplyDataTransformers mReplyDataTransformers;"));
        assertTrue(resStr.contains("/** Operation name for lsfileal. */"));
        assertTrue(resStr.contains("private static final String  OPERATION_NAME = \"lsfileal\";"));
        assertTrue(resStr.contains("/** Properties for operation lsfileal. */"));
        assertTrue(resStr.contains("private static final String  PROGRAM_PROPERTIES = \"lsfileal.properties\";"));
        assertTrue(resStr.contains("public LsfilealProgramInvoker(final String configFileName) {"));
        assertTrue(resStr.contains("mRequestParmsTransformers = new RequestParmsTransformers();"));
        assertTrue(resStr.contains("mReplyDataTransformers = new ReplyDataTransformers();"));
        assertTrue(resStr.contains("* Invoke the LSFILEAL host program."));
        assertTrue(resStr.contains("public final ReplyData lsfileal("));
        assertTrue(resStr.contains("final RequestParms request)"));
        assertTrue(resStr.contains("getRequestParmsTransformers().toHost(request, hostCharset);"));
        assertTrue(resStr.contains("byte[] replyBytes = hostInvoker.invoke(requestID, requestBytes);"));
        assertTrue(resStr.contains("return getReplyDataTransformers().toJava(replyBytes, hostCharset);"));
        
        assertTrue(resStr.contains("* @return the host transformers for RequestParms"));
        assertTrue(resStr.contains("public RequestParmsTransformers getRequestParmsTransformers() {"));
        assertTrue(resStr.contains("return mRequestParmsTransformers;"));
        assertTrue(resStr.contains("* @param transformers the host transformers for RequestParms"));
        assertTrue(resStr.contains("public void setRequestParmsTransformers("));
        assertTrue(resStr.contains("final RequestParmsTransformers transformers) {"));
        assertTrue(resStr.contains("mRequestParmsTransformers = transformers;"));
        assertTrue(resStr.contains("* @return the host transformers for ReplyData"));
        assertTrue(resStr.contains("public ReplyDataTransformers getReplyDataTransformers() {"));
        assertTrue(resStr.contains("return mReplyDataTransformers;"));
        assertTrue(resStr.contains("* @param transformers the host transformers for ReplyData"));
        assertTrue(resStr.contains("public void setReplyDataTransformers("));
        assertTrue(resStr.contains("final ReplyDataTransformers transformers) {"));
        assertTrue(resStr.contains("mReplyDataTransformers = transformers;"));
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
        Jaxws2CixsGenerator.generateProgramInvoker(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "ProgramInvoker.java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileac;"));
        assertTrue(resStr.contains("import java.util.LinkedHashMap;"));
        assertTrue(resStr.contains("import java.util.Map;"));
        assertTrue(resStr.contains("private QueryDataTransformers mQueryDataTransformers;"));
        assertTrue(resStr.contains("private QueryLimitTransformers mQueryLimitTransformers;"));
        assertTrue(resStr.contains("private ReplyDataTransformers mReplyDataTransformers;"));
        assertTrue(resStr.contains("private ReplyStatusTransformers mReplyStatusTransformers;"));
        assertTrue(resStr.contains("/** Properties for operation lsfileac. */"));
        assertTrue(resStr.contains("private static final String  PROGRAM_PROPERTIES = \"lsfileac.properties\";"));
        assertTrue(resStr.contains("public LsfileacProgramInvoker(final String configFileName) {"));
        assertTrue(resStr.contains("mQueryDataTransformers = new QueryDataTransformers();"));
        assertTrue(resStr.contains("mQueryLimitTransformers = new QueryLimitTransformers();"));
        assertTrue(resStr.contains("mReplyDataTransformers = new ReplyDataTransformers();"));
        assertTrue(resStr.contains("mReplyStatusTransformers = new ReplyStatusTransformers();"));
        assertTrue(resStr.contains("mReplyDataTransformers = new ReplyDataTransformers();"));
        assertTrue(resStr.contains("* Invoke the LSFILEAC host program."));
        assertTrue(resStr.contains("public final LsfileacResponseHolder lsfileac("));
        assertTrue(resStr.contains("final LsfileacRequestHolder request)"));
        
        assertTrue(resStr.contains("Map < String, byte[] > requestParts ="));
        assertTrue(resStr.contains("new LinkedHashMap < String, byte[] >();"));
        assertTrue(resStr.contains("requestParts.put(\"QueryData\","));
        assertTrue(resStr.contains("getQueryDataTransformers().toHost("));
        assertTrue(resStr.contains("request.getQueryData(), hostCharset));"));
        assertTrue(resStr.contains("requestParts.put(\"QueryLimit\","));
        assertTrue(resStr.contains("getQueryLimitTransformers().toHost("));
        assertTrue(resStr.contains("request.getQueryLimit(), hostCharset));"));

        assertTrue(resStr.contains("Map < String, byte[] > replyParts ="));
        assertTrue(resStr.contains("hostInvoker.invoke(requestID, requestParts);"));

        assertTrue(resStr.contains("LsfileacResponseHolder reply = new LsfileacResponseHolder();"));
        assertTrue(resStr.contains("reply.setReplyData("));
        assertTrue(resStr.contains("getReplyDataTransformers().toJava("));
        assertTrue(resStr.contains("replyParts.get(\"ReplyData\"), hostCharset));"));
        assertTrue(resStr.contains("reply.setReplyStatus("));
        assertTrue(resStr.contains("getReplyStatusTransformers().toJava("));
        assertTrue(resStr.contains("replyParts.get(\"ReplyStatus\"), hostCharset));"));
        
        assertTrue(resStr.contains("return reply;"));
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
        Jaxws2CixsGenerator.generateProgramInvoker(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "ProgramInvoker.java");

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
        Jaxws2CixsGenerator.generateProgramInvoker(
                operation, getParameters(), operationClassFilesDir);
        String resStr = getSource(
                operationClassFilesDir,
                operation.getClassName() + "ProgramInvoker.java");

        assertFalse(resStr.contains("package "));
    }
}
