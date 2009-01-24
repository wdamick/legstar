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
package com.legstar.cixs.jaxws.gen.model;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.model.AntBuildCixs2JaxwsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.cixs.jaxws.model.CobolHttpClientType;
import com.legstar.cixs.jaxws.model.ProxyTargetType;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation of a generation ant script.
 *
 */
public class AntBuildCixs2JaxwsModelTest extends AbstractTestTemplate {

    /**
     * Generate and test the ant script.
     * @throws Exception if test fails
     */
    public void testCixs2JaxwsBuild() throws Exception {

        AntBuildCixs2JaxwsModel antModel = new AntBuildCixs2JaxwsModel();
        antModel.setProductLocation("/Users/Fady/sandbox/legstar-version");
        antModel.setProbeFile(new File("probe.file.tmp"));

        CixsJaxwsService cixsJaxwsService = Samples.getCultureInfo();

        antModel.setCixsJaxwsService(cixsJaxwsService);
        antModel.setCoxbBinDir(GEN_BIN_DIR);
        antModel.setCustBinDir(GEN_BIN_DIR);
        antModel.setJaxbBinDir(GEN_BIN_DIR);
        antModel.setTargetAntDir(GEN_ANT_DIR);
        antModel.setTargetWarDir(GEN_WAR_DIR);
        antModel.setTargetWDDDir(GEN_WDD_DIR);
        antModel.setTargetCobolDir(GEN_COBOL_DIR);
        antModel.setHostCharset("IBM01147");
        antModel.setSampleCobolHttpClientType(CobolHttpClientType.WEBAPI);
        
        antModel.setProxyTargetType(ProxyTargetType.WEBSERVICE);
        antModel.setWebServiceTargetParameters(Samples.getCultureinfoWebServiceParameters());
        antModel.setHttpTransportParameters(getDefaultHttpParameters(cixsJaxwsService));

        antModel.generateBuild(CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));
        String resStr = getSource(GEN_SRC_DIR, "test.txt");

        assertTrue(resStr.contains("<project basedir=\"/Users/Fady/sandbox/legstar-version\""
                + " default=\"signalSuccess\" name=\"generate-cixs2jaxws\">"));
        assertTrue(resStr.replace('\\', '/').contains("<pathelement location=\"target/src/gen/target/classes\"/>"));
        assertTrue(resStr.contains("<taskdef name=\"cixs2jaxwsgen\""));
        assertTrue(resStr.contains("classname=\"com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator\""));
        assertTrue(resStr.replace('\\', '/').contains("<cixs2jaxwsgen"));
        assertTrue(resStr.replace('\\', '/').contains("targetAntDir=\"target/src/gen/ant\""));
        assertTrue(resStr.replace('\\', '/').contains("targetWDDDir=\"target/src/gen/webapp\""));
        assertTrue(resStr.replace('\\', '/').contains("targetWarDir=\"${env.CATALINA_BASE}/webapp\""));
        assertTrue(resStr.replace('\\', '/').contains("targetCobolDir=\"target/src/gen/cobol\""));
        assertTrue(resStr.replace('\\', '/').contains("jaxbBinDir=\"target/src/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("coxbBinDir=\"target/src/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("custBinDir=\"target/src/gen/target/classes\""));
        assertTrue(resStr.contains("hostCharset=\"IBM01147\""));
        assertTrue(resStr.contains("proxyTargetType=\"WEBSERVICE\""));
        assertTrue(resStr.contains("sampleCobolHttpClientType=\"WEBAPI\""));

        assertTrue(resStr.contains("<cixsJaxwsService name=\"cultureinfo\""));

        assertTrue(resStr.contains("<cixsOperation name=\"getInfo\""));
        assertTrue(resStr.contains("cicsProgramName=\"CULTUREI\""));
        assertTrue(resStr.contains("jaxbType=\"GetInfo\""));
        assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.cultureinfo\""));
        assertTrue(resStr.contains("jaxbType=\"GetInfoResponse\""));
        assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.cultureinfo\""));

        assertTrue(resStr.contains("<webServiceTargetParameters"));
        assertTrue(resStr.contains("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl"));
        assertTrue(resStr.contains("wsdlTargetNamespace=\"http://cultureinfo.cases.test.xsdc.legstar.com/\""));
        assertTrue(resStr.contains("wsdlServiceName=\"CultureInfoImplService\""));
        assertTrue(resStr.contains("wsdlPortName=\"CultureInfoImplPort\""));

        assertTrue(resStr.contains("<httpTransportParameters"));
        assertTrue(resStr.contains("scheme=\"http\""));
        assertTrue(resStr.contains("host=\"192.168.0.4\""));
        assertTrue(resStr.contains("port=\"8080\""));
        assertTrue(resStr.contains("path=\"/c2ws-cultureinfo/cultureinfoProxy\""));
        assertTrue(resStr.contains("userId=\"\""));
        assertTrue(resStr.contains("password=\"\""));

    }

}
