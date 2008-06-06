package com.legstar.c2ws.gen.vm;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.c2ws.gen.C2wsOperationModel;
import com.legstar.c2ws.gen.TestCases;
import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

public class VelocityTemplatesTest extends TestCase {

	/** This generator name. */
	private static final String COBOL_CICS_CLIENT_GENERATOR_NAME =
		"LegStar COBOL CICS client generator";

	/** Parameters expected by the vlc templates.*/
	private Map < String, Object > mParameters;

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(VelocityTemplatesTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/cobol";

	public void setUp() throws Exception {
		CodeGenUtil.initVelocity();
		CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
		CodeGenHelper helper = new CodeGenHelper();
		mParameters = new HashMap < String, Object >();
		mParameters.put("helper", helper);
	}
	
	public void testGenJvmQuery() throws Exception {

		C2wsOperationModel c2wsOperationModel =
			new C2wsOperationModel(TestCases.getJvmqueryOperation());
		
		c2wsOperationModel.setServiceURI("http://192.168.0.5/");
		c2wsOperationModel.setServiceUserId("alice");
		c2wsOperationModel.setServicePassword("inwonderland");
		c2wsOperationModel.setServiceName("jvmquery");
		
		CodeGenUtil.processTemplate(
				COBOL_CICS_CLIENT_GENERATOR_NAME,
				"vlc/c2ws-cobol-cics-client.vm",
				"model", c2wsOperationModel,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, c2wsOperationModel.getCicsProgramName() + ".cbl"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/" + c2wsOperationModel.getCicsProgramName() + ".cbl"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("       PROGRAM-ID. JVMQUERY."));
		assertTrue(resStr.contains("       77  THIS-TRACE-ID               PIC X(13) VALUE 'JVMQUERY'."));
		assertTrue(resStr.contains("77  C2WS-SERVICE-URI            PIC X(19) VALUE"));
		assertTrue(resStr.contains("http://192.168.0.5/'."));
		assertTrue(resStr.contains("77  C2WS-USERID                 PIC X(5) VALUE"));
		assertTrue(resStr.contains("'alice'."));
		assertTrue(resStr.contains("77  C2WS-PASSWORD               PIC X(12) VALUE"));
		assertTrue(resStr.contains("'inwonderland'."));
		assertTrue(resStr.contains("77  C2WS-SERVICE-NAME           PIC X(8) VALUE"));
		assertTrue(resStr.contains("'jvmquery'."));
		assertTrue(resStr.contains("           05 JvmQueryRequest."));
		assertTrue(resStr.contains("               10 envVarNames--C PIC 9(9) BINARY."));
		assertTrue(resStr.contains("               10 envVarNames PIC X(32) OCCURS 0"));
		assertTrue(resStr.contains("                   TO 10 DEPENDING ON envVarNames--C."));
		assertTrue(resStr.contains("           05 JvmQueryReply."));
		assertTrue(resStr.contains("               10 envVarValues--C PIC 9(9) BINARY."));
		assertTrue(resStr.contains("               10 country PIC X(32)."));
		assertTrue(resStr.contains("               10 currencySymbol PIC X(32)."));
		assertTrue(resStr.contains("               10 envVarValues PIC X(32) OCCURS 0"));
		assertTrue(resStr.contains("                   TO 10 DEPENDING ON envVarValues--C."));
		assertTrue(resStr.contains("               10 formattedDate PIC X(32)."));
		assertTrue(resStr.contains("               10 language PIC X(32)."));
		assertTrue(resStr.contains("'JVMQUERY STARTING ==============================='"));
		assertTrue(resStr.contains("'JVMQUERY STOPPING ==============================='"));
		
	}
}
