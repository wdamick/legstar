package com.legstar.coxb.cob2trans;

import java.io.File;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import com.legstar.coxb.cob2trans.Cob2TransGenerator.Cob2TransResult;
import com.legstar.coxb.cob2trans.Cob2TransGenerator.Cob2XsdResult;

/**
 * Test Cob2TransGenerator.
 * 
 */
public class Cob2TransGeneratorTest extends AbstractCob2TransTester {

    /**
     * Check the XSD generation step.
     * 
     * @throws Exception
     *             if test fails
     */
    public void testCob2Xsd() throws Exception {
        Cob2XsdResult cob2xsdResult = Cob2TransGenerator.cob2xsd(_cobolFile,
                null,
                _baseName,
                _dirs.getXsdDir(),
                _context.getCob2XsdModel());
        assertFileContainsAll(cob2xsdResult.xsdFile, new String[] {
                "<cb:cobolElement cobolName=\"CUSTOMER-ID\""
                        + " levelNumber=\"5\" picture=\"9(6)\""
                        + " signed=\"false\"" + " srceLine=\"2\""
                        + " totalDigits=\"6\""
                        + " type=\"ZONED_DECIMAL_ITEM\"/>",
                "<xsd:complexType name=\"PersonalData\">" });
    }

    /**
     * Check the JAXB classes generation step.
     * 
     * @throws Exception
     *             if test fails
     */
    public void testJaxbgen() throws Exception {
        Cob2XsdResult cob2xsdResult = Cob2TransGenerator.cob2xsd(_cobolFile,
                null,
                _baseName,
                _dirs.getXsdDir(),
                _context.getCob2XsdModel());
        File xsdFile = cob2xsdResult.xsdFile;
        Cob2TransGenerator.jaxbgen(xsdFile,
                _dirs.getSrcDir(),
                _context.getCoxbGenModel().getJaxbGenModel());
        assertFileContainsAll(
                new File(
                        "target/gen/" + _baseName
                                + "/src/generated/CustomerData.java"),
                new String[] {
                        "public class CustomerData",
                        "@CobolElement(cobolName = \"CUSTOMER-ID\","
                                + " type = CobolType.ZONED_DECIMAL_ITEM,"
                                + " levelNumber = 5,"
                                + " isSigned = false,"
                                + " totalDigits = 6,"
                                + " picture = \"9(6)\","
                                + " srceLine = 2)" });
    }

    /**
     * Compile JAVA classes.
     * 
     * @throws Exception
     *             if test fails
     */
    public void testCompile() throws Exception {
        Cob2XsdResult cob2xsdResult = Cob2TransGenerator.cob2xsd(_cobolFile,
                null,
                _baseName,
                _dirs.getXsdDir(),
                _context.getCob2XsdModel());
        File xsdFile = cob2xsdResult.xsdFile;
        Cob2TransGenerator.jaxbgen(xsdFile,
                _dirs.getSrcDir(),
                _context.getCoxbGenModel().getJaxbGenModel());
        Cob2TransGenerator.compile(_dirs.getSrcDir(), _dirs.getBinDir(), null,
                true);
        assertTrue(new File("target/gen/" + _baseName
                                + "/bin/generated/CustomerData.class")
                .exists());

        // Try again, forcing the classpath
        Cob2TransGenerator.compile(_dirs.getSrcDir(), _dirs.getBinDir(),
                System.getProperty("java.class.path"), true);
        assertTrue(new File("target/gen/" + _baseName
                                + "/bin/generated/CustomerData.class")
                .exists());
    }

    /**
     * Check the COXB classes generation step.
     * 
     * @throws Exception
     *             if test fails
     */
    public void testCoxbgen() throws Exception {
        Cob2XsdResult cob2xsdResult = Cob2TransGenerator.cob2xsd(_cobolFile,
                null,
                _baseName,
                _dirs.getXsdDir(),
                _context.getCob2XsdModel());
        File xsdFile = cob2xsdResult.xsdFile;
        Cob2TransGenerator.jaxbgen(xsdFile,
                _dirs.getSrcDir(),
                _context.getCoxbGenModel().getJaxbGenModel());
        Cob2TransGenerator.compile(_dirs.getSrcDir(), _dirs.getBinDir(), null,
                true);
        Cob2TransGenerator.coxbgen(xsdFile,
                _context.getCob2XsdModel().getXsdEncoding(),
                _dirs.getSrcDir(),
                _dirs.getBinDir(),
                _context.getCoxbGenModel());
        assertFileContainsAll(
                new File(
                        "target/gen/"
                                + _baseName
                                + "/src/generated/bind/CustomerDataBinding.java"),
                new String[] {
                        "public ICobolZonedDecimalBinding _customerId;",
                        "_customerId = BF.createZonedDecimalBinding(\"CustomerId\","
                        });
    }

    /**
     * Check the jar step.
     * 
     * @throws Exception
     *             if test fails
     */
    public void testJar() throws Exception {
        Cob2XsdResult cob2xsdResult = Cob2TransGenerator.cob2xsd(_cobolFile,
                null,
                _baseName,
                _dirs.getXsdDir(),
                _context.getCob2XsdModel());
        File xsdFile = cob2xsdResult.xsdFile;
        Cob2TransGenerator.jaxbgen(xsdFile,
                _dirs.getSrcDir(),
                _context.getCoxbGenModel().getJaxbGenModel());
        Cob2TransGenerator.compile(_dirs.getSrcDir(), _dirs.getBinDir(), null,
                true);
        File jarFile = Cob2TransGenerator.jar(
                _dirs.getDistDir(),
                _dirs.getBinDir(),
                "mycobol");
        assertEquals("mycobol.jar", jarFile.getName());
        JarFile jarJarFile = new JarFile(jarFile);
        Enumeration < JarEntry > en = jarJarFile.entries();
        while (en.hasMoreElements()) {
            JarEntry jarEntry = en.nextElement();
            assertStringContainsoneOf(jarEntry.getName(),
                    new String[] {
                            "META-INF/MANIFEST.MF",
                            "generated/",
                            "generated/CustomerData.class",
                            "generated/Filler12.class",
                            "generated/ObjectFactory.class",
                            "generated/PersonalData.class",
                            "generated/Transaction.class",
                            "generated/Transactions.class"

                    });
        }
    }

    /**
     * Perform a complete generation for a single COBOL file.
     * 
     * @throws Exception if generation fails
     */
    public void testGenerate() throws Exception {
        Cob2TransModel model = new Cob2TransModel();
        model.getCob2XsdModel().setTargetNamespace(
                "http://legstar.com/test/coxb/cob2trans/" + _baseName);
        model.getCoxbGenModel().setTypeNameSuffix("Type");
        Cob2TransGenerator generator = new Cob2TransGenerator(model);
        Cob2TransResult result = generator.generate(_cobolFile, TARGET_DIR);
        JarFile jarJarFile = new JarFile(result.jarFile);
        Enumeration < JarEntry > en = jarJarFile.entries();
        int entryCount = 0;
        while (en.hasMoreElements()) {
            en.nextElement();
            entryCount++;
        }
        assertEquals(25, entryCount);

    }

    /**
     * Perform a generation with a COBOL file that does not contain complex
     * structures (issue 110).
     */
    public void testGenerateWithNoComplexStructures() {
        try {
            Cob2TransModel model = new Cob2TransModel();
            model.getCob2XsdModel().setTargetNamespace(
                    "http://legstar.com/test/coxb/cob2trans/" + _baseName);
            model.getCoxbGenModel().setTypeNameSuffix("Type");
            Cob2TransGenerator generator = new Cob2TransGenerator(model);
            generator.generate(
                    toFile("        01 A PIC X."), TARGET_DIR);
            fail();
        } catch (Cob2TransException e) {
            assertTrue(e.getMessage().contains(
                    "JAXB Generator did not find any complex types in "));
        }

    }
}
