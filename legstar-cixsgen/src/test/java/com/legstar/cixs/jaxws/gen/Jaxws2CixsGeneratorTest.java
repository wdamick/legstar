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
import java.io.IOException;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;


/**
 * Test cases for Jaxws2CixsGenerator.
 */
public class Jaxws2CixsGeneratorTest extends AbstractTestTemplate {

    /** An instance of the generator. */
    private Jaxws2CixsGenerator mGenerator;

    /** {@inheritDoc} */
    public void setUp() {
        emptyDir(GEN_DIR);
        mGenerator = new Jaxws2CixsGenerator();
        mGenerator.init();
        mGenerator.setJaxbBinDir(JAXB_BIN_DIR);
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
            fail();
        } catch (Exception e) {
            assertEquals("You must provide a service name",
                    e.getCause().getMessage());
        }
        try {
            cixsJaxwsService.setName("jaxwsServiceName");
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetAntDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetAntDir(GEN_ANT_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetWDDDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetWDDDir(GEN_WDD_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetDistDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetDistDir(GEN_DIST_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetWarDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetWarDir(GEN_WAR_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("No operation was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setCixsJaxwsService(Samples.getLsfileae());
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetSrcDir: No directory name was specified",
                    e.getCause().getMessage());
        }
        try {
            generator.setTargetSrcDir(GEN_SRC_DIR);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " TargetBinDir: No directory name was specified",
                    e.getCause().getMessage());
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
     * @param cixsJaxwsService the service descriptor
     */
    private void initJaxwsService(final CixsJaxwsService cixsJaxwsService) {
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
        mGenerator.setTargetSrcDir(GEN_SRC_DIR);
        mGenerator.setTargetBinDir(GEN_BIN_DIR);
        mGenerator.setTargetWarDir(GEN_WAR_DIR);
        mGenerator.setTargetAntDir(
                new File(GEN_ANT_DIR, cixsJaxwsService.getName()));
        mGenerator.setTargetWDDDir(
                new File(GEN_WDD_DIR, cixsJaxwsService.getName()));
        mGenerator.setTargetDistDir(GEN_DIST_DIR);
    }

    /**
     * Check generation for operation with identical input and output structures.
     * @throws Exception if generation fails
     */
    public void testLsfileaeGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileae();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileae", cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileae", "lsfileae", "lsfileae", "Lsfileae");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileae", "lsfileae", "Lsfileae", "Lsfileae", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileae", "lsfileae", "Lsfileae", "Lsfileae", "Response");
    }

    /**
     * Check generation for operation with different input and output structures.
     * @throws Exception if generation fails
     */
    public void testgetLsfilealGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileal();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileal", cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileal", "lsfileal", "lsfileal", "Lsfileal");
        checkWebWrapperResult("/com/legstar/test/cixs/lsfileal", "lsfileal", "lsfileal", "Lsfileal", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/lsfileal", "lsfileal", "lsfileal", "Lsfileal", "Response");
    }

    /**
     * Check generation for CICS containers target component.
     * @throws Exception if generation fails
     */
    public void testLsfileacGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileac();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileac", cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileac", "lsfileac", "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileac", "lsfileac", "lsfileac", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileac", "lsfileac", "lsfileac", "Lsfileac", "Response");
    }

    /**
     * Check generation for multiple operations components.
     * @throws Exception if generation fails
     */
    public void testLsfileaxGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileax();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileax", cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileae", "Lsfileae");
        checkOperationResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileac", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileac", "Lsfileac", "Response");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "Lsfileae", "Lsfileae", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "Lsfileae", "Lsfileae", "Response");
    }

    /**
     * Check generation for service in default package.
     * @throws Exception if generation fails
     */
    public void testLsfileapGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileap();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkServiceArtifacts("", cixsJaxwsService);
        checkOperationResult("", "lsfileap", "lsfileae", "Lsfileae");
    }

    /**
     * Check generation for service with different operation package.
     * @throws Exception if generation fails
     */
    public void testLsfileanGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfilean();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfilean", cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/oper/Lsfilean", "lsfilean", "lsfileae", "Lsfileae");
    }

    /**
     * Check generation for service with different a single structure per container.
     * @throws Exception if generation fails
     */
    public void testLsfileaqGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = Samples.getLsfileaq();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkServiceArtifacts("/com/legstar/test/cixs/Lsfileaq", cixsJaxwsService);
        checkOperationResult("/com/legstar/test/cixs/Lsfileaq", "lsfileaq", "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileaq", "lsfileaq", "lsfileaq", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileaq", "lsfileaq", "lsfileaq", "Lsfileac", "Response");
    }

    /**
     * Check the artifacts generated at the service level.
     * @param relativeLoc where the artifacts should be
     * @param service the legstar service
     * @throws IOException if test fails
     */
    private void checkServiceArtifacts(
            final String relativeLoc,
            final CixsJaxwsService service) throws IOException {

        String resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + service.getInterfaceClassName() + ".java");
        assertTrue(resStr.contains("public interface " + service.getInterfaceClassName() + " {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/"  + service.getInterfaceClassName() + "Impl.java");
        assertTrue(resStr.contains("public class "  + service.getInterfaceClassName()
                + "Impl extends AbstractServiceAdapter implements " + service.getInterfaceClassName() + " {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + service.getInterfaceClassName() + "HostHeader.java");
        assertTrue(resStr.contains("public class " + service.getInterfaceClassName() + "HostHeader {"));

        if (service.getPackageName() != null) {
            resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + "package-info.java");
            assertTrue(resStr.contains("package " + service.getPackageName() + ";"));
    
            resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + "ObjectFactory.java");
            assertTrue(resStr.contains("package " + service.getPackageName() + ";"));
        }

        resStr = getSource(GEN_ANT_DIR, service.getName() + '/' + "build-jar.xml");
        assertTrue(resStr.replace('\\', '/').contains(
                "<jar destfile=\"target/src/gen/target/cixs-" + service.getName() + ".jar\""));

        resStr = getSource(GEN_ANT_DIR, service.getName() + '/' + "build-war.xml");
        assertTrue(resStr.replace('\\', '/').contains(
                "<war warfile=\"target/src/gen/target/cixs-" + service.getName() + ".war\""));

        resStr = getSource(GEN_ANT_DIR, service.getName() + '/' + "deploy.xml");
        assertTrue(resStr.replace('\\', '/').contains(
                "<copy file=\"target/src/gen/target/cixs-" + service.getName() + ".war\""));

        resStr = getSource(GEN_WDD_DIR,  service.getName() + '/' + "web.xml");
        assertTrue(resStr.contains("<servlet-name>" + service.getName() + "Service</servlet-name>"));

        resStr = getSource(GEN_WDD_DIR, service.getName() + '/' + "sun-jaxws.xml");
        assertTrue(resStr.contains("<endpoint name=\"" + service.getName() + "Service\""));
    }

    /**
     * Check the artifacts generated at the operation level.
     * @param relativeLoc where the artifacts should be
     * @param service the service name
     * @param operation the operation name
     * @param className the main java class name
     * @throws IOException if test fails
     */
    public void checkOperationResult(
            final String relativeLoc,
            final String service,
            final String operation,
            final String className) throws IOException {

        String resStr;
        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className + "Exception.java");
        assertTrue(resStr.contains("public class " + className + "Exception"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className + "FaultInfo.java");
        assertTrue(resStr.contains("public class " + className + "FaultInfo {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className + "ProgramInvoker.java");
        assertTrue(resStr.contains("public class " + className
                + "ProgramInvoker extends AbstractProgramInvoker {"));

        resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className + "HostProgram.java");
        assertTrue(resStr.contains("public class " + className
                + "HostProgram extends HostProgram {"));

    }

    /**
     * Check holders for multiple structures.
     * @param relativeLoc where the artifacts should be
     * @param service the service name
     * @param operation the operation name
     * @param className the main java class name
     * @param propertyName holder property name
     * @throws IOException if test fails
     */
    public void checkHolderResult(
            final String relativeLoc,
            final String service,
            final String operation,
            final String className,
            final String propertyName) throws IOException {
        String resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className + propertyName + "Holder.java");
        assertTrue(resStr.contains("public class " + className + propertyName + "Holder"));
    }

    /**
     * Check wrappers for web services.
     * @param relativeLoc where the artifacts should be
     * @param service the service name
     * @param operation the operation name
     * @param className the main java class name
     * @param propertyName wrapper property name
     * @throws IOException if test fails
     */
   public void checkWebWrapperResult(
            final String relativeLoc,
            final String service,
            final String operation,
            final String className,
            final String propertyName) throws IOException {
        String resStr = getSource(GEN_SRC_DIR + relativeLoc + "/" + className + propertyName + ".java");
        assertTrue(resStr.contains("public class " + className + propertyName + " {"));
    }

}
