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
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.DirectoryFileFilter;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.test.cixs.AbstractOperationCases;

/**
 * Test cases for Jaxws2CixsGenerator.
 */
public class Jaxws2CixsGeneratorTest extends AbstractTestTemplate {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** List of COXB cases which require special treatment. */
    private static final List < String > NON_STANDARD_COXB = Arrays
            .asList(new String[] { "lsfileal", "lsfileac", "enumvar",
                    "MSNSearch", "cultureinfo", "jvmquery", "ws", "varar021",
                    "tcobwvb", "perf", "rq071", "redopera", "redmulti",
                    "dplarcht", "redsimpt", "redinout", "redbotha", "coxb137" });

    /** An instance of the generator. */
    private Jaxws2CixsGenerator _generator;

    /** {@inheritDoc} */
    public void setUp() {
        super.setUp();
        setCreateReferences(CREATE_REFERENCES);
        _generator = new Jaxws2CixsGenerator();
        _generator.init();
        _generator.setJaxbBinDir(JAXB_BIN_DIR);
    }

    /**
     * Test get/set certain members.
     */
    public void testGetSet() {
        Jaxws2CixsGenerator generator = new Jaxws2CixsGenerator();
        generator.setJaxbBinDir(new File("jaxb"));
        generator.setCoxbBinDir(new File("coxb"));
        generator.setCustBinDir(new File("cust"));
        assertEquals("jaxb", generator.getJaxbBinDir().toString());
        assertEquals("coxb", generator.getCoxbBinDir().toString());
        assertEquals("cust", generator.getCustBinDir().toString());

    }

    /**
     * Check controls on input make file.
     */
    public void testInputValidation() {
        Jaxws2CixsGenerator generator = new Jaxws2CixsGenerator();
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
            fail();
        } catch (Exception e) {
            assertEquals("You must provide a service name", e.getCause()
                    .getMessage());
        }
        try {
            generator.getCixsJaxwsService().setName("jaxwsServiceName");
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetAntDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetAntDir(GEN_ANT_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetWDDDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetWDDDir(GEN_WDD_DIR);
            generator.execute();
            fail();
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
            generator.setCixsJaxwsService(Samples.getLsfileae());
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetSrcDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetSrcDir(GEN_SRC_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetBinDir: No directory name was specified", e
                    .getCause().getMessage());
        }
        try {
            generator.setTargetBinDir(GEN_BIN_DIR);
            generator.execute();
        } catch (Exception e) {
            fail();
        }

    }

    /**
     * Initialize generator for a given service.
     * 
     * @param cixsJaxwsService the service descriptor
     */
    private void initJaxwsService(final CixsJaxwsService cixsJaxwsService) {
        _generator.setCixsJaxwsService(cixsJaxwsService);
        _generator.setTargetSrcDir(GEN_SRC_DIR);
        _generator.setTargetBinDir(GEN_BIN_DIR);
        _generator.setTargetWarDir(GEN_WAR_DIR);
        _generator.setTargetAntDir(new File(GEN_ANT_DIR, cixsJaxwsService
                .getName()));
        _generator.setTargetWDDDir(new File(GEN_WDD_DIR, cixsJaxwsService
                .getName()));
        _generator.setTargetDistDir(GEN_DIST_DIR);
    }

    /**
     * Test all standard cases.
     * 
     * @throws Exception if generation fails
     */
    public void testAllStandard() throws Exception {
        String[] dirs = COXB_DIR.list(DirectoryFileFilter.INSTANCE);
        for (int i = 0; i < dirs.length; i++) {
            String serviceName = FilenameUtils.getBaseName(dirs[i]);
            if (!NON_STANDARD_COXB.contains(serviceName)) {
                generateAndCheck(serviceName, "Dfhcommarea", "Dfhcommarea",
                        false);
            }
        }
    }

    /**
     * Test LSFILEAL.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileal() throws Exception {
        generateAndCheck("lsfileal", "RequestParms", "ReplyData", false);
    }

    /**
     * Test VARA021.
     * 
     * @throws Exception if generation fails
     */
    public void testVarar021() throws Exception {
        generateAndCheck("varar021", "SearchGrplst", "SearchGrplst", false);
    }

    /**
     * Test LSFILEAC.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileac() throws Exception {
        CixsJaxwsService service = Samples.getLsfileac();
        initJaxwsService(service);
        _generator.execute();
        check(service.getName());
    }

    /**
     * Test LSFILEAX.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileax() throws Exception {
        CixsJaxwsService service = Samples.getLsfileax();
        initJaxwsService(service);
        _generator.execute();
        check(service.getName());
    }

    /**
     * Test REDOPERA.
     * 
     * @throws Exception if generation fails
     */
    public void testRedopera() throws Exception {
        generateAndCheck("redopera", "Dfhcommarea", "Dfhcommarea", true);
    }

    /**
     * Test REDMULTI.
     * 
     * @throws Exception if generation fails
     */
    public void testRedmulti() throws Exception {
        generateAndCheck("redmulti", "Dfhcommarea", "Dfhcommarea", true);
    }

    /**
     * Test DPLARCHT.
     * 
     * @throws Exception if generation fails
     */
    public void testDplarcht() throws Exception {
        generateAndCheck("dplarcht", "Dfhcommarea", "Dfhcommarea", true);
    }

    /**
     * Test REDSIMPT.
     * 
     * @throws Exception if generation fails
     */
    public void testRedsimpt() throws Exception {
        generateAndCheck("redsimpt", "Dfhcommarea", "Dfhcommarea", true);
    }

    /**
     * Test REDINOUT.
     * 
     * @throws Exception if generation fails
     */
    public void testRedinout() throws Exception {
        generateAndCheck("redinout", "Dfhcommarea", "Dfhcommarea", true);
    }

    /**
     * Test REDBOTHA.
     * 
     * @throws Exception if generation fails
     */
    public void testRedbotha() throws Exception {
        generateAndCheck("redbotha", "Dfhcommarea", "Dfhcommarea", true);
    }

    /**
     * Check generation for service in default package.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileap() throws Exception {
        CixsJaxwsService service = Samples.getLsfileap();
        initJaxwsService(service);
        _generator.execute();
        assertTrue(new File(GEN_SRC_DIR, "Lsfileap.java").exists());
        assertTrue(new File(GEN_ANT_DIR, "/lsfileap/build-jar.xml").exists());
        assertTrue(new File(GEN_WDD_DIR, "/lsfileap/sun-jaxws.xml").exists());
    }

    /**
     * Check generation for service with different operation package.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfilean() throws Exception {
        CixsJaxwsService service = Samples.getLsfilean();
        initJaxwsService(service);
        _generator.execute();
        check(service.getName());
        check(new File(REF_SRC_DIR, GEN_SRC_SUBDIR + service.getName()
                + "/oper"),
                new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + service.getName()
                        + "/oper"), "java");
    }

    /**
     * Check generation for service with different a single structure per
     * container.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileac1() throws Exception {
        CixsJaxwsService service = Samples.getLsfileac1();
        initJaxwsService(service);
        _generator.execute();
        check(service.getName());
    }

    /**
     * Check generation for service with multiple structures in commarea.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileam() throws Exception {
        CixsJaxwsService service = Samples.getLsfileam();
        initJaxwsService(service);
        _generator.execute();
        check(service.getName());
    }

    /**
     * Generate and check against the reference.
     * <p/>
     * Assumes a commarea-driven program.
     * 
     * @param serviceName the service name
     * @param inJaxbTypeName the input JAXB type name
     * @param outJaxbTypeName the output JAXB type name
     * @param hasCustomCode true if COXB class references custom code
     * @throws Exception if generation fails
     */
    protected void generateAndCheck(final String serviceName,
            final String inJaxbTypeName, final String outJaxbTypeName,
            final boolean hasCustomCode) throws Exception {

        CixsJaxwsService service = getNewService(serviceName);
        initJaxwsService(service);
        CixsOperation operation = new CixsOperation();
        operation.setName(serviceName);
        operation.setCicsProgramName(serviceName.toUpperCase());
        operation.addInput(AbstractOperationCases.createCixsStructure(
                serviceName, inJaxbTypeName, null, hasCustomCode));
        operation.addOutput(AbstractOperationCases.createCixsStructure(
                serviceName, outJaxbTypeName, null, hasCustomCode));
        service.addCixsOperation(operation);
        _generator.execute();
        check(serviceName);
    }

    /**
     * Check generated artifacts against the reference.
     * 
     * @param serviceName the generated service
     * @throws Exception if can't get reference
     */
    protected void check(final String serviceName) throws Exception {
        check(new File(REF_SRC_DIR, GEN_SRC_SUBDIR + serviceName), new File(
                GEN_SRC_DIR, GEN_SRC_SUBDIR + serviceName), "java");
        check(new File(REF_RES_DIR, serviceName + "/ant"), new File(
                GEN_ANT_DIR, serviceName), "xml");
        check(new File(REF_RES_DIR, serviceName + "/webapp/WEB_INF"), new File(
                GEN_WDD_DIR, serviceName), "xml");
    }
}
