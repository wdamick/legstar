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
package com.legstar.cixs.jaxws.gen;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.cixs.jaxws.model.ProxyTargetType;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test Cixs2JaxwsGenerator.
 *
 */
public class Cixs2JaxwsGeneratorTest extends AbstractTestTemplate {

    /** An instance of the generator. */
    private Cixs2JaxwsGenerator mGenerator;

    /** {@inheritDoc} */
    public void setUp() {
        emptyDir(GEN_DIR);
        mGenerator = new Cixs2JaxwsGenerator();
        mGenerator.init();
        mGenerator.setJaxbBinDir(JAXB_BIN_DIR);
    }

    /**
     * Check input validation.
     */
    public void testInputValidation() {
        Cixs2JaxwsGenerator generator = new Cixs2JaxwsGenerator();
        try {
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException: JaxbBinDir:" 
                    + " No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setJaxbBinDir(JAXB_BIN_DIR);
            generator.execute();
        } catch (Exception e) {
            assertEquals("You must specify a service description",
                    e.getCause().getMessage());
        }
        CixsJaxwsService cixsJaxwsService = new CixsJaxwsService();
        try {
            generator.setCixsJaxwsService(cixsJaxwsService);
            generator.execute();
        } catch (Exception e) {
            assertEquals("You must provide a service name",
                    e.getCause().getMessage());
        }
        try {
            cixsJaxwsService.setName("cixsJaxwsService");
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetAntDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetAntDir(GEN_ANT_DIR);
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetWDDDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetWDDDir(GEN_WDD_DIR);
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:" 
                    + " TargetCobolDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetCobolDir(GEN_COBOL_DIR);
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetWarDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetWarDir(GEN_WAR_DIR);
            generator.execute();
        } catch (Exception e) {
            assertEquals("Missing target Web service WSDL URL",
                    e.getCause().getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlUrl(
                    "http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl");
            generator.execute();
        } catch (Exception e) {
            assertEquals("Missing target Web service namespace",
                    e.getCause().getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlTargetNamespace(
                    "http://cultureinfo.cases.test.xsdc.legstar.com/");
            generator.execute();
        } catch (Exception e) {
            assertEquals("Missing target Web service name",
                    e.getCause().getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlServiceName(
                    "cultureinfoService");
            generator.execute();
        } catch (Exception e) {
            assertEquals("Missing target Web service port name",
                    e.getCause().getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlPortName(
                    "cultureinfoPort");
            generator.execute();
        } catch (Exception e) {
            assertEquals("You must specify an HTTP path",
                    e.getCause().getMessage());
        }
        try {
            generator.getHttpTransportParameters().setPath("trabzon");
            generator.execute();
        } catch (Exception e) {
            assertEquals("The HTTP path must start with the / character",
                    e.getCause().getMessage());
        }
        try {
            generator.getHttpTransportParameters().setPath("/trabzon");
            generator.execute();
        } catch (Exception e) {
            fail(e.getCause().getMessage());
        }

    }

    /**
     * Test a straight generation with web service target.
     * @throws Exception if generation fails
     */
    public void testGenerateWebService() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getCultureInfo();
        initJaxwsService(cixsJaxwsService);
        mGenerator.setProxyTargetType(ProxyTargetType.WEBSERVICE.toString());
        mGenerator.setWebServiceTargetParameters(
                Samples.getCultureinfoWebServiceParameters());
        mGenerator.execute();
        checkAntBuild(cixsJaxwsService.getName());
        checkWebServiceWebDescriptor(cixsJaxwsService.getName());
        checkCobolClient(cixsJaxwsService.getName(),
                cixsJaxwsService.getCixsOperations().get(0).getCicsProgramName());

    }

    /**
     * Test a straight generation with pojo target.
     * @throws Exception if generation fails
     */
    public void testGeneratePojo() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getJvmquery();
        initJaxwsService(cixsJaxwsService);
        mGenerator.setProxyTargetType(ProxyTargetType.POJO.toString());
        mGenerator.setPojoTargetParameters(
                Samples.getJvmqueryPojoParameters());
        mGenerator.execute();
        checkAntBuild(cixsJaxwsService.getName());
        checkPojoWebDescriptor(cixsJaxwsService.getName());
        checkCobolClient(cixsJaxwsService.getName(),
                cixsJaxwsService.getCixsOperations().get(0).getCicsProgramName());

    }
    /**
     * Initialize generator for a given service.
     * @param cixsJaxwsService the service descriptor
     */
    private void initJaxwsService(final CixsJaxwsService cixsJaxwsService) {
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
        mGenerator.setTargetAntDir(
                new File(GEN_ANT_DIR, cixsJaxwsService.getName()));
        mGenerator.setTargetWDDDir(
                new File(GEN_WDD_DIR, cixsJaxwsService.getName()));
        mGenerator.setTargetCobolDir(
                new File(GEN_COBOL_DIR, cixsJaxwsService.getName()));
        mGenerator.setTargetWarDir(GEN_WAR_DIR);
        mGenerator.setHostCharset("IBM01147");
        mGenerator.getHttpTransportParameters().setUserId("alice");
        mGenerator.getHttpTransportParameters().setPassword("inwonderland");

    }

    /**
     * Check the generated ant script.
     * @param service service name
     * @throws Exception if unable to read result
     */
    private void checkAntBuild(final String service) throws Exception {
        String resStr;
        resStr = getSource(
                GEN_ANT_DIR, service + '/' + "build.xml");
        assertTrue(resStr.replace('\\', '/').contains(
                "<war warfile=\"${env.CATALINA_BASE}/webapp/c2ws-" + service + ".war\""));
        assertTrue(resStr.replace('\\', '/').contains("webxml=\"target/src/gen/webapp/" + service + "/web.xml\""));
        assertTrue(resStr.replace('\\', '/').contains("<classes dir=\"target/classes\">"));
        assertTrue(resStr.replace('\\', '/').contains(
                "<include name=\"com/legstar/test/coxb/" + service + "/*.class\"/>"));

    }

    /**
     * Check the generated web descriptor for a web service target.
     * @param service service name
     * @throws Exception if unable to read result
     */
    private void checkWebServiceWebDescriptor(final String service) throws Exception {
        String resStr;
        resStr = getSource(
                GEN_WDD_DIR, service + '/' + "web.xml");
        assertTrue(resStr.contains(
        "<display-name>" + service + "Proxy</display-name>"));
        assertTrue(resStr.contains(
        "<param-value>http://localhost:8080/jaxws-" + service + "/getinfo?wsdl</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>IBM01147</param-value>"));
    }

    /**
     * Check the generated web descriptor for a pojo target.
     * @param service service name
     * @throws Exception if unable to read result
     */
    private void checkPojoWebDescriptor(final String service) throws Exception {
        String resStr;
        resStr = getSource(
                GEN_WDD_DIR, service + '/' + "web.xml");
        assertTrue(resStr.contains(
        "<display-name>" + service + "Proxy</display-name>"));
        assertTrue(resStr.contains(
        "<param-value>com.legstar.proxy.invoke.pojo.PojoInvoker</param-value>"));
        assertTrue(resStr.contains(
        "<param-name>pojoClassName</param-name>"));
        assertTrue(resStr.contains(
        "<param-name>pojoMethodName</param-name>"));
        assertTrue(resStr.contains(
        "<param-value>IBM01147</param-value>"));
    }

    /**
     * Check the generated COBOL client sample.
     * @param service service name
     * @param cicsProgramName COBOL sample name
     * @throws Exception if unable to read result
     */
    private void checkCobolClient(final String service, final String cicsProgramName) throws Exception {
        String resStr;
        resStr = getSource(
                GEN_COBOL_DIR, service + '/' + cicsProgramName + ".cbl");
        String url = "http://" + CodeGenUtil.getLocalIPAddress() + ":8080/c2ws-" + service + "/" + service + "Proxy";

        assertTrue(resStr.contains("       PROGRAM-ID. " + cicsProgramName + "."));
        assertTrue(resStr.contains("77  W00-SERVICE-URI               PIC X(" + url.length() + ") VALUE"));
        assertTrue(resStr.contains("'" + url + "'."));
    }
}
