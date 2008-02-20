package com.legstar.cixs.gen.vm;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cixs.jaxws.gen.CixsHelper;
import com.legstar.cixs.jaxws.gen.CixsJaxwsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

public class MiscVelocityTemplatesTest extends TestCase {
	
    /** Logger. */
	private static final Log LOG = LogFactory.getLog(MiscVelocityTemplatesTest.class);
	
	/** Java Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/java";

	/** Ant scripts will be generated here. */
	private static final String GEN_ANT_DIR = "src/test/gen/ant";

	/** Web descriptors will be generated here. */
	private static final String GEN_WEB_DIR = "src/test/gen/WebContent/WEB-INF";

	/** Property files will be generated here. */
	private static final String GEN_PROP_DIR = "src/test/gen/WebContent/WEB-INF/classes";

	private Map <String, Object> mParameters;
	
	private CixsHelper mCixsHelper;

	public void setUp() throws Exception {
        CodeGenUtil.initVelocity();
       	CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
       	mParameters = new HashMap <String, Object>();
    	CodeGenHelper helper = new CodeGenHelper();
    	mParameters.put("helper", helper);
    	mCixsHelper = new CixsHelper();
    	mParameters.put("cixsHelper", mCixsHelper);

	}
	
	public void testHostHeader() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
    	
		String componentClassFilesLocation = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, jaxwsComponent.getPackageName());
		CixsJaxwsGenerator.generateHeader(
				jaxwsComponent, mParameters, componentClassFilesLocation);
        String resStr = getSource(
        		componentClassFilesLocation,
        		jaxwsComponent.getHeaderClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("@XmlType(name = \"LsfileaeHostHeader\","));
        assertTrue(resStr.contains("namespace = \"http://cixs.test.legstar.com/lsfileae\","));
        assertTrue(resStr.contains("public class LsfileaeHostHeader"));
	}
	
	public void testHolder() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileac();
		CixsOperation operation = jaxwsComponent.getCixsOperations().get(0);
    	
    	String operationNamespace = mCixsHelper.getOperationNamespace(
    			operation, jaxwsComponent.getTargetNamespace());
    	mParameters.put("operationNamespace", operationNamespace);
    	String operationPackageName = mCixsHelper.getOperationPackageName(
    			operation, jaxwsComponent.getPackageName());
    	mParameters.put("operationPackageName", operationPackageName);
    	
    	mParameters.put("propertyName", "Request");

		String operationClassFilesLocation = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, operationPackageName);
		CixsJaxwsGenerator.generateHolders(
				operation, mParameters, operationClassFilesLocation);
        String resStr = getSource(
        		operationClassFilesLocation,
        		operation.getRequestHolderType() + ".java");
        
        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileac;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileac.QueryLimitType;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileac.QueryDataType;"));
        assertTrue(resStr.contains("@XmlType(name = \"LsfileacRequestHolder\","));
        assertTrue(resStr.contains("namespace = \"http://cixs.test.legstar.com/lsfileac\","));
        assertTrue(resStr.contains("\"queryLimit\","));
        assertTrue(resStr.contains("\"queryData\""));
        assertTrue(resStr.contains("public class LsfileacRequestHolder {"));
        assertTrue(resStr.contains("@XmlElement(name = \"queryLimit\","));
        assertTrue(resStr.contains("private QueryLimitType queryLimit;"));
        assertTrue(resStr.contains("@XmlElement(name = \"queryData\","));
        assertTrue(resStr.contains("private QueryDataType queryData;"));
        assertTrue(resStr.contains("public final QueryLimitType getQueryLimit() {"));
        assertTrue(resStr.contains("return queryLimit;"));
        assertTrue(resStr.contains("final QueryLimitType value) {"));
        assertTrue(resStr.contains("queryLimit = value;"));
        assertTrue(resStr.contains("public final QueryDataType getQueryData() {"));
        assertTrue(resStr.contains("return queryData;"));
        assertTrue(resStr.contains("public final void setQueryData("));
        assertTrue(resStr.contains("private QueryDataType queryData;"));
        assertTrue(resStr.contains("final QueryDataType value) {"));
        assertTrue(resStr.contains("queryData = value;"));
	}
	
	public void testWrapper() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
		CixsOperation operation = jaxwsComponent.getCixsOperations().get(0);
		CixsStructure structure = operation.getInput().get(0);
    	
    	String operationNamespace = mCixsHelper.getOperationNamespace(
    			operation, jaxwsComponent.getTargetNamespace());
    	mParameters.put("operationNamespace", operationNamespace);
    	String operationPackageName = mCixsHelper.getOperationPackageName(
    			operation, jaxwsComponent.getPackageName());
    	mParameters.put("operationPackageName", operationPackageName);
    	mParameters.put("propertyName", "Request");
    	mParameters.put("fieldName", "request");
    	mParameters.put("wrapperType", operation.getRequestWrapperType());
    	mParameters.put("importType", structure.getJaxbPackageName()
				+ '.' + structure.getJaxbType());
    	mParameters.put("fieldType", structure.getJaxbType());

		String operationClassFilesLocation = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, operationPackageName);
		CixsJaxwsGenerator.generateWrappers(
				operation, mParameters, operationClassFilesLocation);
        String resStr = getSource(
        		operationClassFilesLocation,
        		operation.getRequestWrapperType() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.DfhcommareaType;"));
        assertTrue(resStr.contains("@XmlType(name = \"LsfileaeRequest\","));
        assertTrue(resStr.contains("namespace = \"http://cixs.test.legstar.com/lsfileae\","));
        assertTrue(resStr.contains("\"request\""));
        assertTrue(resStr.contains("public class LsfileaeRequest {"));
        assertTrue(resStr.contains("@XmlElement(name = \"Request\","));
        assertTrue(resStr.contains("private DfhcommareaType request;"));
        assertTrue(resStr.contains("public final DfhcommareaType getRequest() {"));
        assertTrue(resStr.contains("return request;"));
        assertTrue(resStr.contains("public final void setRequest("));
        assertTrue(resStr.contains("final DfhcommareaType value) {"));
        assertTrue(resStr.contains("request = value;"));
	}

	public void testFault() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
		CixsOperation operation = jaxwsComponent.getCixsOperations().get(0);
    	
    	String operationNamespace = mCixsHelper.getOperationNamespace(
    			operation, jaxwsComponent.getTargetNamespace());
    	mParameters.put("operationNamespace", operationNamespace);
    	String operationPackageName = mCixsHelper.getOperationPackageName(
    			operation, jaxwsComponent.getPackageName());
    	mParameters.put("operationPackageName", operationPackageName);

		String operationClassFilesLocation = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, operationPackageName);
		CixsJaxwsGenerator.generateFault(
				operation, mParameters, operationClassFilesLocation);
        String resStr = getSource(
        		operationClassFilesLocation,
        		operation.getFaultType() + ".java");
        
        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("@WebFault(name = \"LsfileaeFaultInfo\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\")"));
        assertTrue(resStr.contains("public class LsfileaeFault"));
        assertTrue(resStr.contains("private LsfileaeFaultInfo faultInfo;"));
        assertTrue(resStr.contains("public LsfileaeFault("));
        assertTrue(resStr.contains("final LsfileaeFaultInfo fault) {"));
        assertTrue(resStr.contains("final LsfileaeFaultInfo fault,"));
        assertTrue(resStr.contains("returns fault bean: com.legstar.test.cixs.lsfileae.LsfileaeFaultInfo"));
        assertTrue(resStr.contains("public final LsfileaeFaultInfo getFaultInfo() {"));
	}
	
	public void testFaultInfo() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
		CixsOperation operation = jaxwsComponent.getCixsOperations().get(0);
    	
    	String operationNamespace = mCixsHelper.getOperationNamespace(
    			operation, jaxwsComponent.getTargetNamespace());
    	mParameters.put("operationNamespace", operationNamespace);
    	String operationPackageName = mCixsHelper.getOperationPackageName(
    			operation, jaxwsComponent.getPackageName());
    	mParameters.put("operationPackageName", operationPackageName);

		String operationClassFilesLocation = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, operationPackageName);
		CixsJaxwsGenerator.generateFaultInfo(
				operation, mParameters, operationClassFilesLocation);
        String resStr = getSource(
        		operationClassFilesLocation,
        		operation.getFaultInfoType() + ".java");
        
        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("public class LsfileaeFaultInfo {"));
        assertTrue(resStr.contains("@XmlType(name = \"LsfileaeFaultInfo\","));
        assertTrue(resStr.contains("namespace = \"http://cixs.test.legstar.com/lsfileae\","));
	}
	
	public void testProgramPropertiesCommarea() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileal();
		CixsOperation operation = jaxwsComponent.getCixsOperations().get(0);
    	
    	String operationNamespace = mCixsHelper.getOperationNamespace(
    			operation, jaxwsComponent.getTargetNamespace());
    	mParameters.put("operationNamespace", operationNamespace);
    	String operationPackageName = mCixsHelper.getOperationPackageName(
    			operation, jaxwsComponent.getPackageName());
    	mParameters.put("operationPackageName", operationPackageName);

		String operationPropertiesFilesLocation = GEN_PROP_DIR + '/' + jaxwsComponent.getName();
		CodeGenUtil.checkDirectory(operationPropertiesFilesLocation, true);
		CixsJaxwsGenerator.generateProgramProperties(
				operation, mParameters, operationPropertiesFilesLocation);
        String resStr = getSource(
        		operationPropertiesFilesLocation,
        		operation.getCicsProgramName() + ".properties");

        assertTrue(resStr.contains("CICSProgramName=LSFILEAL"));
        assertTrue(resStr.contains("CICSLength=8043"));
        assertTrue(resStr.contains("CICSDataLength=20"));
	}
	
	public void testProgramPropertiesContainer() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileac();
		CixsOperation operation = jaxwsComponent.getCixsOperations().get(0);
    	
    	String operationNamespace = mCixsHelper.getOperationNamespace(
    			operation, jaxwsComponent.getTargetNamespace());
    	mParameters.put("operationNamespace", operationNamespace);
    	String operationPackageName = mCixsHelper.getOperationPackageName(
    			operation, jaxwsComponent.getPackageName());
    	mParameters.put("operationPackageName", operationPackageName);

		String operationPropertiesFilesLocation = GEN_PROP_DIR + '/' + jaxwsComponent.getName();
		CodeGenUtil.checkDirectory(operationPropertiesFilesLocation, true);
		CixsJaxwsGenerator.generateProgramProperties(
				operation, mParameters, operationPropertiesFilesLocation);
        String resStr = getSource(
        		operationPropertiesFilesLocation,
        		operation.getCicsProgramName() + ".properties");

        assertTrue(resStr.contains("CICSProgramName=LSFILEAC"));
        assertTrue(resStr.contains("CICSChannel=LSFILEAC-CHANNEL"));
        assertTrue(resStr.contains("CICSInContainers_1=QueryLimit"));
        assertTrue(resStr.contains("CICSInContainersLength_1=10"));
        assertTrue(resStr.contains("CICSInContainers_2=QueryData"));
        assertTrue(resStr.contains("CICSInContainersLength_2=48"));
        assertTrue(resStr.contains("CICSOutContainers_1=ReplyData"));
        assertTrue(resStr.contains("CICSOutContainersLength_1=7905"));
        assertTrue(resStr.contains("CICSOutContainers_2=ReplyStatus"));
        assertTrue(resStr.contains("CICSOutContainersLength_2=151"));
	}
	
	public void testSunJaxwsXml() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
    	
		String componentWebFilesLocation = GEN_WEB_DIR + '/' + jaxwsComponent.getName();
		CodeGenUtil.checkDirectory(componentWebFilesLocation, true);
		CixsJaxwsGenerator.generateSunJaxwsXml(
				jaxwsComponent, mParameters, componentWebFilesLocation);
        String resStr = getSource(
        		componentWebFilesLocation,
        		"sun-jaxws.xml");

        assertTrue(resStr.contains("<endpoint name=\"lsfileaeService\""));
        assertTrue(resStr.contains("implementation=\"com.legstar.test.cixs.lsfileae.LsfileaeImpl\""));
        assertTrue(resStr.contains("url-pattern=\"/lsfileae\"/>"));
	}

	public void testWebXml() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
    	
		String componentWebFilesLocation = GEN_WEB_DIR + '/' + jaxwsComponent.getName();
		CodeGenUtil.checkDirectory(componentWebFilesLocation, true);
		CixsJaxwsGenerator.generateWebXml(
				jaxwsComponent, mParameters, componentWebFilesLocation);
        String resStr = getSource(
        		componentWebFilesLocation,
        		"web.xml");

        assertTrue(resStr.contains("<display-name>LegStar Jaxws lsfileae</display-name>"));
        assertTrue(resStr.contains("<description>LegStar Jaxws lsfileae Web Service</description>"));
        assertTrue(resStr.contains("<servlet-name>lsfileaeService</servlet-name>"));
        assertTrue(resStr.contains("<url-pattern>/lsfileae</url-pattern>"));
	}

	public void testAntBuildWar() throws Exception {
		
		CixsJaxwsService jaxwsComponent = TestCases.getLsfileae();
    	
		mParameters.put("warDir", "/Servers/TOMDev/webapps");
		mParameters.put("wddDir", "/Legsem/Legstar/Dev/WebContent/WEB-INF");
		mParameters.put("jaxbBinDir", "/legstar-jaxbgen-cases/target/classes");
		mParameters.put("coxbBinDir", "/legstar-coxbgen-cases/target/classes");
		mParameters.put("cixsBinDir", "/legstar-cixsgen-cases/target/classes");
		mParameters.put("custBinDir", "/legstar-cixsgen-cust-cases/target/classes");
		mParameters.put("propDir", "/Legsem/Legstar/Dev/WebContent/WEB-INF/classes");

		String componentAntFilesLocation = GEN_ANT_DIR + '/' + jaxwsComponent.getName();
		CodeGenUtil.checkDirectory(componentAntFilesLocation, true);
		CixsJaxwsGenerator.generateAntBuildWar(
				jaxwsComponent, mParameters, componentAntFilesLocation);
        String resStr = getSource(
        		componentAntFilesLocation,
        		"build.xml");

        assertTrue(resStr.contains("<delete file=\"/Servers/TOMDev/webapps/cixs-lsfileae.war\" includeEmptyDirs=\"true\""));
        assertTrue(resStr.contains("<war warfile=\"/Servers/TOMDev/webapps/cixs-lsfileae.war\""));
        assertTrue(resStr.contains("webxml=\"/Legsem/Legstar/Dev/WebContent/WEB-INF/lsfileae/web.xml\">"));
        assertTrue(resStr.contains("<webinf dir=\"/Legsem/Legstar/Dev/WebContent/WEB-INF/lsfileae\""));
        assertTrue(resStr.contains("<classes dir=\"/legstar-jaxbgen-cases/target/classes\">"));
        assertTrue(resStr.contains("<include name=\"com/legstar/test/coxb/lsfileae/*.class\"/>"));
        assertTrue(resStr.contains("<classes dir=\"/legstar-coxbgen-cases/target/classes\">"));
        assertTrue(resStr.contains("<include name=\"com/legstar/test/coxb/lsfileae/bind/*.class\"/>"));
        assertTrue(resStr.contains("<classes dir=\"/legstar-cixsgen-cases/target/classes\">"));
        assertTrue(resStr.contains("<classes dir=\"/legstar-cixsgen-cust-cases/target/classes\">"));
	}

	private String getSource(String srcLocation, String srcName) throws Exception {
        BufferedReader in = new BufferedReader(new FileReader(srcLocation + '/' + srcName));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            LOG.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        return resStr;
	}

}
