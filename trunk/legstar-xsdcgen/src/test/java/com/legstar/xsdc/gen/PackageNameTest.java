package com.legstar.xsdc.gen;

/**
 * Check package name handling.
 *
 */
public class PackageNameTest extends AbstractTest {

    /**
     * Target name is mixed case.
     * @throws Exception if generation fails
     */
    public void testMixedCaseName() throws Exception {
        XsdCobolAnnotator xsdCobolAnnotator = createXsdCobolAnnotator();
        xsdCobolAnnotator.setInputXsdUri(getSchemaFileURI("singleSimpleElement.xsd"));
        xsdCobolAnnotator.execute();
        /* Read the resulting output source*/
        String result = getSource(GEN_DIR, "singleSimpleElement.xsd");
        assertTrue(result.contains("<jaxb:package name=\"com.example.finance.creditcardfaults.xsd\"/>"));
    }

}
