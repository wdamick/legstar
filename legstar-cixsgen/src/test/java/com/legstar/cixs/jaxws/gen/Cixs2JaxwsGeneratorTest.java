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
package com.legstar.cixs.jaxws.gen;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;

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
            assertEquals("You must specify a valid URI",
                    e.getCause().getMessage());
        }
        try {
            cixsJaxwsService.setServiceURI(
                    cixsJaxwsService.getDefaultServiceURI());
            generator.execute();
        } catch (Exception e) {
            assertEquals("You must specify a target Web Service WSDL URL",
                    e.getCause().getMessage());
        }
        try {
            cixsJaxwsService.setWsdlUrl("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl");
            generator.execute();
        } catch (Exception e) {
            fail(e.getCause().getMessage());
        }

    }

    /**
     * Test a straight generation.
     * @throws Exception if generation fails
     */
    public void testGenerate() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getCultureInfo();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkAntBuild(cixsJaxwsService.getName());
        checkWebDescriptor(cixsJaxwsService.getName());
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
        cixsJaxwsService.setServiceUserId("alice");
        cixsJaxwsService.setServicePassword("inwonderland");
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
        assertTrue(resStr.replace('\\', '/').contains("webxml=\"target/src/gen/webapp/cultureinfo/web.xml\""));
        assertTrue(resStr.replace('\\', '/').contains("<classes dir=\"target/classes\">"));
        assertTrue(resStr.replace('\\', '/').contains("<include name=\"com/legstar/test/coxb/cultureinfo/*.class\"/>"));

    }

    /**
     * Check the generated web descriptor.
     * @param service service name
     * @throws Exception if unable to read result
     */
    private void checkWebDescriptor(final String service) throws Exception {
        String resStr;
        resStr = getSource(
                GEN_WDD_DIR, service + '/' + "web.xml");
        assertTrue(resStr.contains(
        "<display-name>cultureinfoProxy</display-name>"));
        assertTrue(resStr.contains(
        "<param-value>http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>http://cultureinfo.cases.test.xsdc.legstar.com/</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>CultureInfoImplPort</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>CultureInfoImplService</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>GetInfo</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>com.legstar.test.coxb.cultureinfo</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>GetInfoResponse</param-value>"));
        assertTrue(resStr.contains(
        "<param-value>com.legstar.test.coxb.cultureinfo</param-value>"));
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
        assertTrue(resStr.contains("PROGRAM-ID. CULTUREI."));
        assertTrue(resStr.contains("77  C2WS-SERVICE-URI            PIC X(55) VALUE"));
        assertTrue(resStr.contains("'http://localhost:8080/c2ws-cultureinfo/cultureinfoProxy'"));
        assertTrue(resStr.contains("'alice'"));
        assertTrue(resStr.contains("'inwonderland'"));
        assertTrue(resStr.contains("'cultureinfo'."));
        assertTrue(resStr.contains("02 GetInfo."));
        assertTrue(resStr.contains("04 cultureCode PIC X(32)."));
        assertTrue(resStr.contains("02 GetInfoResponse."));
        assertTrue(resStr.contains("04 currencySymbol PIC X(32)."));
        assertTrue(resStr.contains("05 cultureCode PIC X(32)."));
    }
}
