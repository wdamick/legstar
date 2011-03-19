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
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.DirectoryFileFilter;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
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
     * Check generation for operation with identical input and output
     * structures.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileaeGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileae();
        initJaxwsService(cixsJaxwsService);
        _generator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileae",
                cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileae", "lsfileae",
                "lsfileae", "Lsfileae");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileae", "lsfileae",
                "Lsfileae", "Lsfileae", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileae", "lsfileae",
                "Lsfileae", "Lsfileae", "Response");
    }

    /**
     * Check generation for operation with different input and output
     * structures.
     * 
     * @throws Exception if generation fails
     */
    public void testgetLsfilealGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileal();
        initJaxwsService(cixsJaxwsService);
        _generator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileal",
                cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileal", "lsfileal",
                "lsfileal", "Lsfileal");
        checkWebWrapperResult("/com/legstar/test/cixs/lsfileal", "lsfileal",
                "lsfileal", "Lsfileal", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/lsfileal", "lsfileal",
                "lsfileal", "Lsfileal", "Response");
    }

    /**
     * Check generation for CICS containers target component.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileacGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileac();
        initJaxwsService(cixsJaxwsService);
        _generator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileac",
                cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileac", "lsfileac",
                "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileac", "lsfileac",
                "lsfileac", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileac", "lsfileac",
                "lsfileac", "Lsfileac", "Response");
    }

    /**
     * Check generation for multiple operations components.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileaxGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileax();
        initJaxwsService(cixsJaxwsService);
        _generator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileax",
                cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileax", "lsfileax",
                "lsfileae", "Lsfileae");
        checkOperationResult("/com/legstar/test/cixs/Lsfileax", "lsfileax",
                "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileax", "lsfileax",
                "lsfileac", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileax", "lsfileax",
                "lsfileac", "Lsfileac", "Response");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileax", "lsfileax",
                "Lsfileae", "Lsfileae", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileax", "lsfileax",
                "Lsfileae", "Lsfileae", "Response");
    }

    /**
     * Check generation for service in default package.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileapGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileap();
        initJaxwsService(cixsJaxwsService);
        _generator.execute();
        checkServiceArtifacts("", cixsJaxwsService);
        checkOperationResult("", "lsfileap", "lsfileae", "Lsfileae");
    }

    /**
     * Check generation for service with different operation package.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileanGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfilean();
        initJaxwsService(cixsJaxwsService);
        _generator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfilean",
                cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/oper/Lsfilean",
                "lsfilean", "lsfileae", "Lsfileae");
    }

    /**
     * Check generation for service with different a single structure per
     * container.
     * 
     * @throws Exception if generation fails
     */
    public void testLsfileac1GenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileac1();
        initJaxwsService(cixsJaxwsService);
        _generator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileac1",
                cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileac1", "lsfileac1",
                "lsfileac", "Lsfileac");
    }

    /**
     * Check the artifacts generated at the service level.
     * 
     * @param relativeLoc where the artifacts should be
     * @param service the legstar service
     * @throws IOException if test fails
     */
    private void checkServiceArtifacts(final String relativeLoc,
            final CixsJaxwsService service) throws IOException {

        String resStr = getSource(GEN_SRC_DIR + relativeLoc + "/"
                + service.getInterfaceClassName() + ".java");
        assertTrue(resStr.contains("public interface "
                + service.getInterfaceClassName() + " {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/"
                + service.getInterfaceClassName() + "Impl.java");
        assertTrue(resStr.contains("public class "
                + service.getInterfaceClassName()
                + "Impl extends AbstractServiceAdapter implements "
                + service.getInterfaceClassName() + " {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/"
                + service.getInterfaceClassName() + "HostHeader.java");
        assertTrue(resStr.contains("public class "
                + service.getInterfaceClassName() + "HostHeader {"));

        if (service.getPackageName() != null) {
            resStr = getSource(GEN_SRC_DIR + relativeLoc + "/"
                    + "package-info.java");
            assertTrue(resStr.contains("package " + service.getPackageName()
                    + ";"));

            resStr = getSource(GEN_SRC_DIR + relativeLoc + "/"
                    + "ObjectFactory.java");
            assertTrue(resStr.contains("package " + service.getPackageName()
                    + ";"));
        }

        resStr = getSource(GEN_ANT_DIR, service.getName() + '/'
                + "build-jar.xml");
        assertTrue(resStr.replace('\\', '/').contains(
                "<jar destfile=\"target/src/gen/target/cixs-"
                        + service.getName() + ".jar\""));

        resStr = getSource(GEN_ANT_DIR, service.getName() + '/'
                + "build-war.xml");
        assertTrue(resStr.replace('\\', '/').contains(
                "<war warfile=\"target/src/gen/target/cixs-"
                        + service.getName() + ".war\""));

        resStr = getSource(GEN_ANT_DIR, service.getName() + '/' + "deploy.xml");
        assertTrue(resStr.replace('\\', '/').contains(
                "<copy file=\"target/src/gen/target/cixs-" + service.getName()
                        + ".war\""));

        resStr = getSource(GEN_WDD_DIR, service.getName() + '/' + "web.xml");
        assertTrue(resStr.contains("<servlet-name>" + service.getName()
                + "Service</servlet-name>"));

        resStr = getSource(GEN_WDD_DIR, service.getName() + '/'
                + "sun-jaxws.xml");
        assertTrue(resStr.contains("<endpoint name=\"" + service.getName()
                + "Service\""));
    }

    /**
     * Check the artifacts generated at the operation level.
     * 
     * @param relativeLoc where the artifacts should be
     * @param service the service name
     * @param operation the operation name
     * @param className the main java class name
     * @throws IOException if test fails
     */
    public void checkOperationResult(final String relativeLoc,
            final String service, final String operation, final String className)
            throws IOException {

        String resStr;
        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className
                + "Exception.java");
        assertTrue(resStr.contains("public class " + className + "Exception"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className
                + "FaultInfo.java");
        assertTrue(resStr.contains("public class " + className + "FaultInfo {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className
                + "ProgramInvoker.java");
        assertTrue(resStr.contains("public class " + className
                + "ProgramInvoker extends AbstractProgramInvoker {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className
                + "HostProgram.java");
        assertTrue(resStr.contains("public class " + className
                + "HostProgram extends HostProgram {"));

    }

    /**
     * Check holders for multiple structures.
     * 
     * @param relativeLoc where the artifacts should be
     * @param service the service name
     * @param operation the operation name
     * @param className the main java class name
     * @param propertyName holder property name
     * @throws IOException if test fails
     */
    public void checkHolderResult(final String relativeLoc,
            final String service, final String operation,
            final String className, final String propertyName)
            throws IOException {
        String resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className
                + propertyName + "Holder.java");
        assertTrue(resStr.contains("public class " + className + propertyName
                + "Holder"));
    }

    /**
     * Check wrappers for web services.
     * 
     * @param relativeLoc where the artifacts should be
     * @param service the service name
     * @param operation the operation name
     * @param className the main java class name
     * @param propertyName wrapper property name
     * @throws IOException if test fails
     */
    public void checkWebWrapperResult(final String relativeLoc,
            final String service, final String operation,
            final String className, final String propertyName)
            throws IOException {
        String resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className
                + propertyName + ".java");
        assertTrue(resStr.contains("public class " + className + propertyName
                + " {"));
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
                System.out.println(dirs[i]);
                CixsJaxwsService service = getNewService(serviceName);
                initJaxwsService(service);
                CixsOperation operation = new CixsOperation();
                operation.setName(serviceName);
                operation.setCicsProgramName(serviceName.toUpperCase());
                CixsStructure commarea = new CixsStructure();
                commarea.setJaxbType("Dfhcommarea");
                commarea.setJaxbPackageName(AbstractOperationCases.JAXB_PKG_PREFIX
                        + "." + serviceName);
                operation.addInput(commarea);
                operation.addOutput(commarea);
                service.addCixsOperation(operation);
                _generator.execute();
                check(new File(SRC_REF_DIR, GEN_SRC_SUBDIR + serviceName),
                        new File(GEN_SRC_DIR, GEN_SRC_SUBDIR + serviceName),
                        "java");
                check(new File(ANT_REF_DIR, serviceName), new File(GEN_ANT_DIR,
                        serviceName), "xml");
                check(new File(WDD_REF_DIR, "WEB-INF/" + serviceName),
                        new File(GEN_WDD_DIR, "WEB-INF/" + serviceName), "xml");
            }
        }
    }
}
