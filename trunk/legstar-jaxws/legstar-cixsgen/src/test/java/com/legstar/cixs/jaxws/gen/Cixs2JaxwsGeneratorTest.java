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
package com.legstar.cixs.jaxws.gen;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.gen.model.options.ProxyTargetType;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;

/**
 * Test Cixs2JaxwsGenerator.
 * 
 */
public class Cixs2JaxwsGeneratorTest extends AbstractTestTemplate {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** An instance of the generator. */
    private Cixs2JaxwsGenerator _generator;

    /** {@inheritDoc} */
    public void setUp() {
        emptyDir(GEN_DIR);
        setCreateReferences(CREATE_REFERENCES);
        _generator = new Cixs2JaxwsGenerator();
        _generator.init();
        _generator.setJaxbBinDir(JAXB_BIN_DIR);
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
                    + " No directory name was specified", e.getCause()
                    .getMessage());
        }
        try {
            generator.setJaxbBinDir(JAXB_BIN_DIR);
            generator.execute();
            generator.execute();
        } catch (Exception e) {
            assertEquals("You must provide a service name", e.getCause()
                    .getMessage());
        }
        try {
            generator.getCixsJaxwsService().setName("cixsJaxwsService");
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetAntDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetAntDir(GEN_ANT_DIR);
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetWDDDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetWDDDir(GEN_WDD_DIR);
            generator.execute();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetDistDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetDistDir(GEN_DIST_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetWarDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetWarDir(GEN_WAR_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("No operation was specified", e.getCause()
                    .getMessage());
        }
        try {
            generator.setCixsJaxwsService(Samples.getJvmquery());
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetCobolDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetCobolDir(GEN_COBOL_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("Missing target Web service WSDL URL", e.getCause()
                    .getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlUrl(
                    "http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl");
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("Missing target Web service namespace", e.getCause()
                    .getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlTargetNamespace(
                    "http://cultureinfo.cases.test.xsdc.legstar.com/");
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("Missing target Web service name", e.getCause()
                    .getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlServiceName(
                    "cultureinfoService");
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("Missing target Web service port name", e.getCause()
                    .getMessage());
        }
        try {
            generator.getWebServiceTargetParameters().setWsdlPortName(
                    "cultureinfoPort");
            generator.execute();
        } catch (Exception e) {
            fail(e.getCause().getMessage());
        }

    }

    /**
     * Initialize generator for a given service.
     * 
     * @param cixsJaxwsService the service descriptor
     */
    private void initJaxwsService(final CixsJaxwsService cixsJaxwsService) {
        _generator.setCixsJaxwsService(cixsJaxwsService);
        _generator.setTargetAntDir(new File(GEN_ANT_DIR, cixsJaxwsService
                .getName()));
        _generator.setTargetWDDDir(new File(GEN_WDD_DIR, cixsJaxwsService
                .getName()));
        _generator.setTargetCobolDir(new File(GEN_COBOL_DIR, cixsJaxwsService
                .getName()));
        _generator.setTargetWarDir(GEN_WAR_DIR);
        _generator.setTargetDistDir(GEN_DIST_DIR);
        _generator.setHostCharset("IBM01147");
        _generator.getHttpTransportParameters().setUserId("alice");
        _generator.getHttpTransportParameters().setPassword("inwonderland");

    }

    /**
     * Test a straight generation with web service target.
     * 
     * @throws Exception if generation fails
     */
    public void testCultureinfo() throws Exception {
        CixsJaxwsService service = Samples.getCultureInfo();
        initJaxwsService(service);
        _generator.setProxyTargetType(ProxyTargetType.WEBSERVICE.toString());
        _generator.setWebServiceTargetParameters(Samples
                .getCultureinfoWebServiceParameters());
        _generator.execute();
        check(service.getName());

    }

    /**
     * Test a straight generation with complex web service target.
     * 
     * @throws Exception if generation fails
     */
    public void testMSNSearch() throws Exception {
        CixsJaxwsService service = Samples.getMSNSearch();
        initJaxwsService(service);
        _generator.setProxyTargetType(ProxyTargetType.WEBSERVICE.toString());
        _generator.setWebServiceTargetParameters(Samples
                .getMSNSearchWebServiceParameters());
        _generator.execute();
        check(service.getName());

    }

    /**
     * Test a straight generation with pojo target.
     * 
     * @throws Exception if generation fails
     */
    public void testJvmquery() throws Exception {
        CixsJaxwsService service = Samples.getJvmquery();
        initJaxwsService(service);
        _generator.setProxyTargetType(ProxyTargetType.POJO.toString());
        _generator.setPojoTargetParameters(Samples.getJvmqueryPojoParameters());
        _generator.execute();
        check(service.getName());

    }

    /**
     * Test a straight generation with web service target.
     * 
     * @throws Exception if generation fails
     */
    public void testJvmqueryWs() throws Exception {
        CixsJaxwsService service = Samples.getJvmqueryWs();
        initJaxwsService(service);
        _generator.setProxyTargetType(ProxyTargetType.WEBSERVICE.toString());
        _generator.setWebServiceTargetParameters(Samples
                .getJvmqueryWebServiceParameters());
        _generator.execute();
        check(service.getName());

    }

    /**
     * Check generated artifacts against the reference.
     * 
     * @param serviceName the generated service
     * @throws Exception if can't get reference
     */
    protected void check(final String serviceName) throws Exception {
        check(new File(REF_RES_DIR, serviceName + "/ant"), new File(
                GEN_ANT_DIR, serviceName), "xml");
        check(new File(REF_RES_DIR, serviceName + "/webapp/WEB_INF"), new File(
                GEN_WDD_DIR, serviceName), "xml");
        check(new File(REF_RES_DIR, serviceName + "/cobol"), new File(
                GEN_COBOL_DIR, serviceName), "cbl");
    }
}
