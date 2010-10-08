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
import com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test velocity templates that are specific to proxies.
 *
 */
public class Cixs2JaxwsVelocityTemplatesTest extends AbstractTestTemplate {

    /**
     * Test the generation of web.xml and a target web service.
     * @throws Exception  if generation fails
     */
    public void testWebXmlGenerationWebService() throws Exception {

        CixsJaxwsService model = Samples.getCultureInfo();
        getParameters().put("hostCharset", "IBM01147");
        getParameters().put("proxyTargetType", "webservice");
        Samples.getCultureinfoWebServiceParameters().add(getParameters());
        
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.SERVICE_WEB_XML_VLC_TEMPLATE,
                GEN_WDD_DIR, "web.xml");
        assertTrue(resStr.contains("<display-name>cultureinfoProxy</display-name>"));
        assertTrue(resStr.contains(
                "<description>cultureinfoProxy is a proxy for webservice cultureinfo</description>"));
        assertTrue(resStr.contains("<servlet-class>com.legstar.c2ws.servlet.C2wsProxy</servlet-class>"));
        assertTrue(resStr.contains("<param-name>operationProxyClassName</param-name>"));
        assertTrue(resStr.contains("<param-value>com.legstar.proxy.invoke.DirectOperationProxy</param-value>"));
        assertTrue(resStr.contains("<param-name>requestTransformersClassName</param-name>"));
        assertTrue(resStr.contains(
                "<param-value>com.legstar.test.coxb.cultureinfo.bind.GetInfoTransformers</param-value>"));
        assertTrue(resStr.contains("<param-name>responseTransformersClassName</param-name>"));
        assertTrue(resStr.contains(
                "<param-value>com.legstar.test.coxb.cultureinfo.bind.GetInfoResponseTransformers</param-value>"));
        assertTrue(resStr.contains("<param-name>wsdlUrl</param-name>"));
        assertTrue(resStr.contains("<param-value>http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl</param-value>"));
        assertTrue(resStr.contains("<param-name>wsdlTargetNamespace</param-name>"));
        assertTrue(resStr.contains("<param-value>http://cultureinfo.cases.test.xsdc.legstar.com/</param-value>"));
        assertTrue(resStr.contains("<param-name>wsdlPortName</param-name"));
        assertTrue(resStr.contains("<param-value>CultureInfoImplPort</param-value>"));
        assertTrue(resStr.contains("<param-name>wsdlServiceName</param-name>"));
        assertTrue(resStr.contains("<param-value>CultureInfoImplService</param-value>"));
        assertTrue(resStr.contains("<param-name>requestJaxbType</param-name>"));
        assertTrue(resStr.contains("<param-value>GetInfo</param-value>"));
        assertTrue(resStr.contains("<param-name>requestJaxbPackageName</param-name>"));
        assertTrue(resStr.contains("<param-value>com.legstar.test.coxb.cultureinfo</param-value>"));
        assertTrue(resStr.contains("<param-name>responseJaxbType</param-name>"));
        assertTrue(resStr.contains("<param-value>GetInfoResponse</param-value>"));
        assertTrue(resStr.contains("<param-name>responseJaxbPackageName</param-name>"));
        assertTrue(resStr.contains("<param-value>com.legstar.test.coxb.cultureinfo</param-value>"));
        assertTrue(resStr.contains("<param-name>hostCharset</param-name>"));
        assertTrue(resStr.contains("<param-value>IBM01147</param-value>"));
        assertTrue(resStr.contains("<url-pattern>/cultureinfoProxy</url-pattern>"));
    }

    /**
     * Test the generation of web.xml and a target POJO.
     * @throws Exception  if generation fails
     */
    public void testWebXmlGenerationPojo() throws Exception {

        CixsJaxwsService model = Samples.getJvmquery();
        getParameters().put("hostCharset", "IBM01147");
        getParameters().put("proxyTargetType", "pojo");
        Samples.getJvmqueryPojoParameters().add(getParameters());
        
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.SERVICE_WEB_XML_VLC_TEMPLATE,
                GEN_WDD_DIR, "web.xml");
        assertTrue(resStr.contains("<display-name>jvmqueryProxy</display-name>"));
        assertTrue(resStr.contains(
                "<description>jvmqueryProxy is a proxy for pojo jvmquery</description>"));
        assertTrue(resStr.contains("<servlet-class>com.legstar.c2ws.servlet.C2wsProxy</servlet-class>"));
        assertTrue(resStr.contains("<param-name>operationProxyClassName</param-name>"));
        assertTrue(resStr.contains("<param-value>com.legstar.proxy.invoke.DirectOperationProxy</param-value>"));
        assertTrue(resStr.contains("<param-name>requestTransformersClassName</param-name>"));
        assertTrue(resStr.contains(
                "<param-value>com.legstar.test.coxb.jvmquery.bind.JvmQueryRequestTransformers</param-value>"));
        assertTrue(resStr.contains("<param-name>responseTransformersClassName</param-name>"));
        assertTrue(resStr.contains(
                "<param-value>com.legstar.test.coxb.jvmquery.bind.JvmQueryReplyTransformers</param-value>"));
        assertTrue(resStr.contains("<param-name>hostCharset</param-name>"));
        assertTrue(resStr.contains("<param-value>IBM01147</param-value>"));

        assertTrue(resStr.contains("<!--  Parameters for POJO invoker -->"));
        assertTrue(resStr.contains("<param-name>proxyInvokerClassName</param-name>"));
        assertTrue(resStr.contains("<param-value>com.legstar.proxy.invoke.pojo.PojoInvoker</param-value>"));
        assertTrue(resStr.contains("<param-name>pojoClassName</param-name>"));
        assertTrue(resStr.contains("<param-value>com.legstar.xsdc.test.cases.jvmquery.JVMQuery</param-value>"));
        assertTrue(resStr.contains("<param-name>pojoMethodName</param-name>"));
        assertTrue(resStr.contains("<param-value>queryJvm</param-value>"));
        
        
        assertTrue(resStr.contains("<url-pattern>/jvmqueryProxy</url-pattern>"));
    }

    /**
     * Test the generated war file build ant script.
     * @throws Exception if test fails
     */
    public void testAntWarBuildGeneration() throws Exception {

        getParameters().put("jaxbBinDir", "target/classes");
        getParameters().put("coxbBinDir", "target/classes");
        getParameters().put("targetWarDir", "/Servers/TOMDev/webapps");
        getParameters().put("targetDistDir", "target/dist");
        getParameters().put("targetWDDDir", "/Legsem/Legstar/Dev/webapp/WEB-INF");

        CixsJaxwsService model = Samples.getCultureInfo();
        String resStr = genSource(model,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME,
                Cixs2JaxwsGenerator.SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE,
                GEN_ANT_DIR, "build.xml");
        assertTrue(resStr.contains("<war warfile=\"target/dist/c2ws-cultureinfo.war\""));
        assertTrue(resStr.contains("webxml=\"/Legsem/Legstar/Dev/webapp/WEB-INF/web.xml\">"));
        assertTrue(resStr.contains("<classes dir=\"target/classes\">"));
        assertTrue(resStr.contains("<include name=\"com/legstar/test/coxb/cultureinfo/*.class\"/>"));
    }

    /**
     * War deploy ant script.
     * @throws Exception if test fails
     */
    public void testAntDeploy() throws Exception {

        CixsJaxwsService jaxwsComponent = Samples.getCultureInfo();
 
        getParameters().put("targetWarDir", "/Servers/TOMDev/webapps");
        getParameters().put("targetWDDDir", "/Legsem/Legstar/Dev/webapp/WEB-INF");
        getParameters().put("targetBinDir", "/legstar-cixsgen-cases/target/classes");
        getParameters().put("targetDistDir", "/legstar-cixsgen-cases/target");
        getParameters().put("jaxbBinDir", "/legstar-jaxbgen-cases/target/classes");
        getParameters().put("coxbBinDir", "/legstar-coxbgen-cases/target/classes");
        getParameters().put("custBinDir", "/legstar-cixsgen-cust-cases/target/classes");

        File componentAntFilesDir =
            new File(GEN_ANT_DIR, jaxwsComponent.getName());
        CodeGenUtil.checkDirectory(componentAntFilesDir, true);
        String filename = Cixs2JaxwsGenerator.generateAntDeploy(
                jaxwsComponent, getParameters(), componentAntFilesDir);
        String resStr = getSource(componentAntFilesDir, filename);

        assertTrue(resStr.contains("<delete file=\"/Servers/TOMDev/webapps/c2ws-cultureinfo.war\""));
        assertTrue(resStr.contains("<copy file=\"/legstar-cixsgen-cases/target/c2ws-cultureinfo.war\""));
        assertTrue(resStr.contains("todir=\"/Servers/TOMDev/webapps\"/>"));
    }

}
