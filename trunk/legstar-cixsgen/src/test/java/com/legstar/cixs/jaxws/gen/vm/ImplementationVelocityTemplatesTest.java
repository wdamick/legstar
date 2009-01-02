/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
     * @throws Exception if test fails
     */
    public void testImplementationCommareainEqCommareaout() throws Exception {

        CixsJaxwsService model = Samples.getLsfileae();

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(
                model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
                componentClassFilesDir,
                model.getImplementationClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.Dfhcommarea;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.bind.DfhcommareaBinding;"));
        assertTrue(resStr.contains("public LsfileaeImpl()"));
        assertTrue(resStr.contains("@WebService(endpointInterface = \"com.legstar.test.cixs.lsfileae.Lsfileae\","));
        assertTrue(resStr.contains("serviceName = \"lsfileaeService\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\")"));
        assertTrue(resStr.contains("public class LsfileaeImpl implements Lsfileae {"));
        assertTrue(resStr.contains("private static final String  LSFILEAE_PROP_FILE = \"lsfileae.properties\";"));
        assertTrue(resStr.contains("public final Dfhcommarea lsfileae("));
        assertTrue(resStr.contains("final Dfhcommarea request,"));
        assertTrue(resStr.contains("final LsfileaeHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileaeException {"));
        assertTrue(resStr.contains("Dfhcommarea reply = null;"));
        assertTrue(resStr.contains("String requestID = \"lsfileae\";"));
        assertTrue(resStr.contains("mConfigFileName, getAddress(hostHeader), LSFILEAE_PROP_FILE);"));
        assertTrue(resStr.contains("DfhcommareaBinding inputDfhcommareaBinding ="));
        assertTrue(resStr.contains("new DfhcommareaBinding(request);"));
        assertTrue(resStr.contains("DfhcommareaBinding outputDfhcommareaBinding ="));
        assertTrue(resStr.contains("new DfhcommareaBinding();"));
        assertTrue(resStr.contains("inputDfhcommareaBinding,"));
        assertTrue(resStr.contains("outputDfhcommareaBinding);"));
        assertTrue(resStr.contains("reply = outputDfhcommareaBinding.getDfhcommarea();"));
        assertTrue(resStr.contains("* @throws LsfileaeException the fault exception"));
        assertTrue(resStr.contains("private void reportLsfileaeException("));
        assertTrue(resStr.contains("final String text) throws LsfileaeException {"));
        assertTrue(resStr.contains("LsfileaeFaultInfo faultInfo = new LsfileaeFaultInfo();"));
        assertTrue(resStr.contains("+ \"lsfileae\""));
        assertTrue(resStr.contains("+ \"com.legstar.test.cixs.lsfileae\");"));
        assertTrue(resStr.contains("throw (new LsfileaeException(text + ' '"));
        assertTrue(resStr.contains("private LegStarAddress getAddress("));
        assertTrue(resStr.contains("final LsfileaeHostHeader hostHeader) {"));
    }

    /**
     * Case of a commarea driven target program with input layout different from output layout.
     * @throws Exception if test fails
     */
    public void testInterfaceCommareainNeqCommareaout() throws Exception {

        CixsJaxwsService model = Samples.getLsfileal();

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(
                model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
                componentClassFilesDir,
                model.getImplementationClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileal;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.RequestParms;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.ReplyData;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.bind.RequestParmsBinding;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.bind.ReplyDataBinding;"));
        assertTrue(resStr.contains("public LsfilealImpl()"));
        assertTrue(resStr.contains("@WebService(endpointInterface = \"com.legstar.test.cixs.lsfileal.Lsfileal\","));
        assertTrue(resStr.contains("serviceName = \"lsfilealService\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileal\")"));
        assertTrue(resStr.contains("public class LsfilealImpl implements Lsfileal {"));
        assertTrue(resStr.contains("private static final String  LSFILEAL_PROP_FILE = \"lsfileal.properties\";"));
        assertTrue(resStr.contains("public final ReplyData lsfileal("));
        assertTrue(resStr.contains("final RequestParms request,"));
        assertTrue(resStr.contains("final LsfilealHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfilealException {"));
        assertTrue(resStr.contains("ReplyData reply = null;"));
        assertTrue(resStr.contains("String requestID = \"lsfileal\";"));
        assertTrue(resStr.contains("mConfigFileName, getAddress(hostHeader), LSFILEAL_PROP_FILE);"));
        assertTrue(resStr.contains("RequestParmsBinding inputRequestParmsBinding ="));
        assertTrue(resStr.contains("new RequestParmsBinding(request);"));
        assertTrue(resStr.contains("ReplyDataBinding outputReplyDataBinding ="));
        assertTrue(resStr.contains("new ReplyDataBinding();"));
        assertTrue(resStr.contains("inputRequestParmsBinding,"));
        assertTrue(resStr.contains("outputReplyDataBinding);"));
        assertTrue(resStr.contains("reply = outputReplyDataBinding.getReplyData();"));
        assertTrue(resStr.contains("* @throws LsfilealException the fault exception"));
        assertTrue(resStr.contains("private void reportLsfilealException("));
        assertTrue(resStr.contains("final String text) throws LsfilealException {"));
        assertTrue(resStr.contains("LsfilealFaultInfo faultInfo = new LsfilealFaultInfo();"));
        assertTrue(resStr.contains("+ \"lsfileal\""));
        assertTrue(resStr.contains("+ \"com.legstar.test.cixs.lsfileal\");"));
        assertTrue(resStr.contains("throw (new LsfilealException(text + ' '"));
        assertTrue(resStr.contains("private LegStarAddress getAddress("));
        assertTrue(resStr.contains("final LsfilealHostHeader hostHeader) {"));
    }

    /**
     * Case of a container driven target program.
     * @throws Exception if test fails
     */
    public void testImplementationContainer() throws Exception {

        CixsJaxwsService model = Samples.getLsfileac();

        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
                GEN_SRC_DIR, model.getPackageName(), true);
        Jaxws2CixsGenerator.generateImplementation(
                model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
                componentClassFilesDir,
                model.getImplementationClassName() + ".java");

        assertTrue(resStr.contains("import java.util.LinkedHashMap;"));
        assertTrue(resStr.contains("import java.util.Map;"));
        assertTrue(resStr.contains("import com.legstar.coxb.ICobolComplexBinding;"));
        assertTrue(resStr.contains("LsfileacResponseHolder reply = null;"));
        assertTrue(resStr.contains("Map <String, ICobolComplexBinding> inputParts ="));
        assertTrue(resStr.contains("new LinkedHashMap <String, ICobolComplexBinding>();"));
        assertTrue(resStr.contains("QueryLimitBinding inputQueryLimitBinding ="));
        assertTrue(resStr.contains("new QueryLimitBinding(request.getQueryLimit());"));
        assertTrue(resStr.contains("inputParts.put(\"QueryLimit\", inputQueryLimitBinding);"));
        assertTrue(resStr.contains("QueryDataBinding inputQueryDataBinding ="));
        assertTrue(resStr.contains("new QueryDataBinding(request.getQueryData());"));
        assertTrue(resStr.contains("inputParts.put(\"QueryData\", inputQueryDataBinding);"));
        assertTrue(resStr.contains("Map <String, ICobolComplexBinding> outputParts ="));
        assertTrue(resStr.contains("new LinkedHashMap <String, ICobolComplexBinding>();"));
        assertTrue(resStr.contains("ReplyDataBinding outputReplyDataBinding ="));
        assertTrue(resStr.contains("new ReplyDataBinding();"));
        assertTrue(resStr.contains("outputParts.put(\"ReplyData\", outputReplyDataBinding);"));
        assertTrue(resStr.contains("new ReplyDataBinding();"));
        assertTrue(resStr.contains("ReplyStatusBinding outputReplyStatusBinding ="));
        assertTrue(resStr.contains("new ReplyStatusBinding();"));
        assertTrue(resStr.contains("outputParts.put(\"ReplyStatus\", outputReplyStatusBinding);"));
        assertTrue(resStr.contains("String requestID = \"lsfileac\";"));
        assertTrue(resStr.contains("inputParts,"));
        assertTrue(resStr.contains("outputParts);"));
        assertTrue(resStr.contains("reply = new LsfileacResponseHolder();"));
        assertTrue(resStr.contains("reply.setReplyData(outputReplyDataBinding.getReplyData());"));
        assertTrue(resStr.contains("reply.setReplyStatus(outputReplyStatusBinding.getReplyStatus());"));
    }

}
