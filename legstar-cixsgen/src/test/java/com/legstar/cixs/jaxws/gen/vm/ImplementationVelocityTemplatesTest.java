package com.legstar.cixs.jaxws.gen.vm;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;


public class ImplementationVelocityTemplatesTest extends AbstractTestTemplate {
	
	public void testImplementationCommareainEqCommareaout() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, jaxwsComponent.getPackageName(), true);
		Jaxws2CixsGenerator.generateImplementation(
				jaxwsComponent, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		jaxwsComponent.getImplementationClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.DfhcommareaType;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding;"));
        assertTrue(resStr.contains("public LsfileaeImpl()"));
        assertTrue(resStr.contains("@WebService(endpointInterface = \"com.legstar.test.cixs.lsfileae.Lsfileae\","));
        assertTrue(resStr.contains("serviceName = \"lsfileaeService\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\")"));
        assertTrue(resStr.contains("public class LsfileaeImpl implements Lsfileae {"));
        assertTrue(resStr.contains("private static final String  LSFILEAE_PROP_FILE = \"lsfileae.properties\";"));
        assertTrue(resStr.contains("public final DfhcommareaType lsfileae("));
        assertTrue(resStr.contains("final DfhcommareaType request,"));
        assertTrue(resStr.contains("final LsfileaeHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileaeFault {"));
        assertTrue(resStr.contains("DfhcommareaType reply = null;"));
        assertTrue(resStr.contains("String requestID = \"lsfileae\";"));
        assertTrue(resStr.contains("mConfigFileName, getAddress(hostHeader), LSFILEAE_PROP_FILE);"));
        assertTrue(resStr.contains("DfhcommareaTypeBinding inputDfhcommareaTypeBinding ="));
        assertTrue(resStr.contains("new DfhcommareaTypeBinding(request);"));
        assertTrue(resStr.contains("DfhcommareaTypeBinding outputDfhcommareaTypeBinding ="));
        assertTrue(resStr.contains("new DfhcommareaTypeBinding();"));
        assertTrue(resStr.contains("inputDfhcommareaTypeBinding,"));
        assertTrue(resStr.contains("outputDfhcommareaTypeBinding);"));
        assertTrue(resStr.contains("reply = outputDfhcommareaTypeBinding.getDfhcommareaType();"));
        assertTrue(resStr.contains("* @throws LsfileaeFault the fault exception"));
        assertTrue(resStr.contains("private void reportLsfileaeFault("));
        assertTrue(resStr.contains("final String text) throws LsfileaeFault {"));
        assertTrue(resStr.contains("LsfileaeFaultInfo faultInfo = new LsfileaeFaultInfo();"));
        assertTrue(resStr.contains("+ \"lsfileae\""));
        assertTrue(resStr.contains("+ \"com.legstar.test.cixs.lsfileae\");"));
        assertTrue(resStr.contains("throw (new LsfileaeFault(text + ' '"));
        assertTrue(resStr.contains("private LegStarAddress getAddress("));
        assertTrue(resStr.contains("final LsfileaeHostHeader hostHeader) {"));
	}
	
	public void testInterfaceCommareainNeqCommareaout() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileal();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, jaxwsComponent.getPackageName(), true);
		Jaxws2CixsGenerator.generateImplementation(
				jaxwsComponent, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		jaxwsComponent.getImplementationClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileal;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.RequestParmsType;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.ReplyDataType;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.bind.RequestParmsTypeBinding;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.bind.ReplyDataTypeBinding;"));
        assertTrue(resStr.contains("public LsfilealImpl()"));
        assertTrue(resStr.contains("@WebService(endpointInterface = \"com.legstar.test.cixs.lsfileal.Lsfileal\","));
        assertTrue(resStr.contains("serviceName = \"lsfilealService\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileal\")"));
        assertTrue(resStr.contains("public class LsfilealImpl implements Lsfileal {"));
        assertTrue(resStr.contains("private static final String  LSFILEAL_PROP_FILE = \"lsfileal.properties\";"));
        assertTrue(resStr.contains("public final ReplyDataType lsfileal("));
        assertTrue(resStr.contains("final RequestParmsType request,"));
        assertTrue(resStr.contains("final LsfilealHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfilealFault {"));
        assertTrue(resStr.contains("ReplyDataType reply = null;"));
        assertTrue(resStr.contains("String requestID = \"lsfileal\";"));
        assertTrue(resStr.contains("mConfigFileName, getAddress(hostHeader), LSFILEAL_PROP_FILE);"));
        assertTrue(resStr.contains("RequestParmsTypeBinding inputRequestParmsTypeBinding ="));
        assertTrue(resStr.contains("new RequestParmsTypeBinding(request);"));
        assertTrue(resStr.contains("ReplyDataTypeBinding outputReplyDataTypeBinding ="));
        assertTrue(resStr.contains("new ReplyDataTypeBinding();"));
        assertTrue(resStr.contains("inputRequestParmsTypeBinding,"));
        assertTrue(resStr.contains("outputReplyDataTypeBinding);"));
        assertTrue(resStr.contains("reply = outputReplyDataTypeBinding.getReplyDataType();"));
        assertTrue(resStr.contains("* @throws LsfilealFault the fault exception"));
        assertTrue(resStr.contains("private void reportLsfilealFault("));
        assertTrue(resStr.contains("final String text) throws LsfilealFault {"));
        assertTrue(resStr.contains("LsfilealFaultInfo faultInfo = new LsfilealFaultInfo();"));
        assertTrue(resStr.contains("+ \"lsfileal\""));
        assertTrue(resStr.contains("+ \"com.legstar.test.cixs.lsfileal\");"));
        assertTrue(resStr.contains("throw (new LsfilealFault(text + ' '"));
        assertTrue(resStr.contains("private LegStarAddress getAddress("));
        assertTrue(resStr.contains("final LsfilealHostHeader hostHeader) {"));
	}
	
	public void testImplementationContainer() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileac();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, jaxwsComponent.getPackageName(), true);
		Jaxws2CixsGenerator.generateImplementation(
				jaxwsComponent, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		jaxwsComponent.getImplementationClassName() + ".java");

        assertTrue(resStr.contains("import java.util.LinkedHashMap;"));
        assertTrue(resStr.contains("import java.util.Map;"));
        assertTrue(resStr.contains("import com.legstar.coxb.ICobolComplexBinding;"));
        assertTrue(resStr.contains("LsfileacResponseHolder reply = null;"));
        assertTrue(resStr.contains("Map <String, ICobolComplexBinding> inputParts ="));
        assertTrue(resStr.contains("new LinkedHashMap <String, ICobolComplexBinding>();"));
        assertTrue(resStr.contains("QueryLimitTypeBinding inputQueryLimitTypeBinding ="));
        assertTrue(resStr.contains("new QueryLimitTypeBinding(request.getQueryLimit());"));
        assertTrue(resStr.contains("inputParts.put(\"QueryLimit\", inputQueryLimitTypeBinding);"));
        assertTrue(resStr.contains("QueryDataTypeBinding inputQueryDataTypeBinding ="));
        assertTrue(resStr.contains("new QueryDataTypeBinding(request.getQueryData());"));
        assertTrue(resStr.contains("inputParts.put(\"QueryData\", inputQueryDataTypeBinding);"));
        assertTrue(resStr.contains("Map <String, ICobolComplexBinding> outputParts ="));
        assertTrue(resStr.contains("new LinkedHashMap <String, ICobolComplexBinding>();"));
        assertTrue(resStr.contains("ReplyDataTypeBinding outputReplyDataTypeBinding ="));
        assertTrue(resStr.contains("new ReplyDataTypeBinding();"));
        assertTrue(resStr.contains("outputParts.put(\"ReplyData\", outputReplyDataTypeBinding);"));
        assertTrue(resStr.contains("new ReplyDataTypeBinding();"));
        assertTrue(resStr.contains("ReplyStatusTypeBinding outputReplyStatusTypeBinding ="));
        assertTrue(resStr.contains("new ReplyStatusTypeBinding();"));
        assertTrue(resStr.contains("outputParts.put(\"ReplyStatus\", outputReplyStatusTypeBinding);"));
        assertTrue(resStr.contains("String requestID = \"lsfileac\";"));
        assertTrue(resStr.contains("inputParts,"));
        assertTrue(resStr.contains("outputParts);"));
        assertTrue(resStr.contains("reply = new LsfileacResponseHolder();"));
        assertTrue(resStr.contains("reply.setReplyData(outputReplyDataTypeBinding.getReplyDataType());"));
        assertTrue(resStr.contains("reply.setReplyStatus(outputReplyStatusTypeBinding.getReplyStatusType());"));
	}
	
}
