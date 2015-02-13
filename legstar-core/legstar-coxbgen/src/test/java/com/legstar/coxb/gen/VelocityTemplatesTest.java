/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.gen;

import java.io.File;

import org.apache.commons.io.FileUtils;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;

/**
 * Test the individual velocity templates.
 * 
 */
public class VelocityTemplatesTest extends AbstractCoxbGenTest {

    /** This generator name. */
    private static final String BINDING_GENERATOR_NAME = "LegStar Binding generator";

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** @{inheritDoc */
    public void setUp() throws Exception {
        super.setUp();
        setCreateReferences(CREATE_REFERENCES);
    }

    /**
     * A complex type case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenAllTypes() throws Exception {
        genSourceAndCheck(CoxbGenWriter.COMPLEX_VLC_TEMPLATE, "alltypes",
                "Dfhcommarea", "DfhcommareaBinding.java");
    }

    /**
     * A complex array type case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenArrayssm() throws Exception {
        genSourceAndCheck(CoxbGenWriter.COMPLEX_VLC_TEMPLATE, "arrayssm",
                "Dfhcommarea", "DfhcommareaBinding.java");
    }

    /**
     * A complex array wrapper type case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenArrayssmWrapper() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("arrayssm", "Dfhcommarea");
        ICobolArrayComplexBinding ca = (ICobolArrayComplexBinding) ce
                .getChildrenList().get(1);
        genSource(CoxbGenWriter.COMPLEX_ARRAY_VLC_TEMPLATE, "arrayssm", ca,
                "TableComplexWrapper", "TableComplexWrapperBinding.java");
        check("arrayssm", "TableComplexWrapperBinding.java");
    }

    /**
     * A complex type containing a variable size array case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenArraysdo() throws Exception {
        genSourceAndCheck(CoxbGenWriter.COMPLEX_VLC_TEMPLATE, "arraysdo",
                "Dfhcommarea", "DfhcommareaBinding.java");
    }

    /**
     * A complex type containing a redefine case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenRedsimpt() throws Exception {
        genSourceAndCheck(CoxbGenWriter.COMPLEX_VLC_TEMPLATE, "redsimpt",
                "Dfhcommarea", "DfhcommareaBinding.java");
    }

    /**
     * A choice type case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenRedsimptChoice() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("redsimpt", "Dfhcommarea");
        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList()
                .get(0);
        genSource(CoxbGenWriter.CHOICE_VLC_TEMPLATE, "redsimpt", cc,
                "CDefinition1Choice", "CDefinition1ChoiceBinding.java");
        check("redsimpt", "CDefinition1ChoiceBinding.java");
    }

    /**
     * A choice strategy type case.
     * <p/>
     * Be careful not to overwrite the reference choice selectors which have
     * been customized. So we use a local reference.
     * 
     * @throws Exception if generation fails
     */
    public void testGenRedsimptChoiceStrategy() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("redsimpt", "Dfhcommarea");
        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList()
                .get(0);
        getParameters().put("choice-strategy-type", "Unmarshal");
        getParameters().put("choice-strategy-qualified-class-name",
                "com.legstar.coxb.cust.redsimpt.ChoiceSelector");
        File resultFolder = new File(GEN_SRC_DIR,
                "com/legstar/coxb/cust/redsimpt");
        File refFolder = new File(REF_DIR, getClass().getSimpleName());

        genSource(CoxbGenWriter.CHOICE_STRATEGY_VLC_TEMPLATE, "redsimpt", cc,
                "CDefinition1Choice", "ChoiceSelector.java",
                createModel("redsimpt"), resultFolder);
        check("redsimpt", "ChoiceSelector.java", refFolder, resultFolder);

    }

    /**
     * A binding to a POJO case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenJvmQueryReply() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("jvmquery", "JvmQueryReply");
        String packageName = JAXB_PKG_PFX + "." + "jvmquery";
        CoxbGenModel coxbContext = new CoxbGenModel();
        coxbContext.setJaxbPackageName(packageName);
        coxbContext.setCoxbPackageName(packageName + ".bind");
        coxbContext
                .setAlternativePackageName("com.legstar.xsdc.test.cases.jvmquery");
        coxbContext.setAlternativeFactoryName("ObjectFactory");
        genSource(CoxbGenWriter.COMPLEX_VLC_TEMPLATE, "jvmquery", ce,
                "JvmQueryReply", "JvmQueryReplyBinding.java", coxbContext);
        check("jvmquery", "JvmQueryReplyBinding.java");

    }

    /**
     * Test JAXB options in ANT script.
     * 
     * @throws Exception if generation fails
     */
    public void testJaxbAntOptions() throws Exception {
        String packageName = JAXB_PKG_PFX + "." + "lsfileae";
        CoxbGenModel coxbContext = new CoxbGenModel();
        coxbContext.setJaxbPackageName(packageName);
        coxbContext.setCoxbPackageName(packageName + ".bind");
        coxbContext.setEciCompatible(true);
        coxbContext.setNoPackageInfo(true);

        File resultFile = CodeGenUtil.getFile(GEN_ANT_DIR, "build.xml");
        CodeGenUtil.processTemplate(BINDING_GENERATOR_NAME,
                CoxbGenModel.COXB_VELOCITY_MACRO_NAME, "antModel", coxbContext,
                getParameters(), resultFile);

        String result = FileUtils.readFileToString(resultFile);
        assertTrue(result.contains("eciCompatible=\"true\""));
        assertTrue(result.contains("noPackageInfo=\"true\""));

    }

    /**
     * A POJO with boolean field.
     * http://code.google.com/p/legstar/issues/detail?id=137.
     * 
     * @throws Exception if generation fails
     */
    public void testPojoBoolean() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("coxb137", "BoolPojo");
        String packageName = JAXB_PKG_PFX + "." + "coxb137";
        CoxbGenModel coxbContext = new CoxbGenModel();
        coxbContext.setJaxbPackageName(packageName);
        coxbContext.setCoxbPackageName(packageName + ".bind");
        genSource(CoxbGenWriter.COMPLEX_VLC_TEMPLATE, "coxb137", ce,
                "BoolPojo", "BoolPojoBinding.java", coxbContext);
        String result = FileUtils.readFileToString(new File(GEN_SRC_DIR,
                coxbContext.getCoxbPackageName().replace(".", "/") + "/"
                        + "BoolPojoBinding.java"));
        assertFalse(result.contains("import boolean;"));
        assertTrue(result
                .contains("+ \" value=\" + mValueObject.isABoolean());"));
        assertTrue(result
                .contains("_aBoolean.setObjectValue(mValueObject.isABoolean());"));
        assertTrue(result
                .contains("mValueObject.setABoolean((Boolean) bindingValue);"));

    }

    /**
     * A COBOL field with both REDEFINES and OCCURS.
     * http://code.google.com/p/legstar/issues/detail?id=177.
     * 
     * @throws Exception if generation fails
     */
    public void testRedefinesAndOccurs() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("coxb177", "Dfhcommarea");
        String packageName = JAXB_PKG_PFX + "." + "coxb177";
        CoxbGenModel coxbContext = new CoxbGenModel();
        coxbContext.setJaxbPackageName(packageName);
        coxbContext.setCoxbPackageName(packageName + ".bind");
        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList()
                .get(0);
        genSource(CoxbGenWriter.CHOICE_VLC_TEMPLATE, "coxb177", cc,
                "SpfRecordDataChoice", "SpfRecordDataChoiceBinding.java",
                coxbContext);
        String result = FileUtils.readFileToString(new File(GEN_SRC_DIR,
                coxbContext.getCoxbPackageName().replace(".", "/") + "/"
                        + "SpfRecordDataChoiceBinding.java"));
        assertTrue(result
                .contains("public ICobolComplexBinding _spfBucketTableWrapperItem;"));

    }

    /**
     * Generate a host to java transformer case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenHostToJavaTransformer() throws Exception {
        genSourceAndCheck(CoxbGenWriter.HOST_TO_JAVA_XFORMER_VLC_TEMPLATE,
                "lsfileae", "Dfhcommarea",
                "DfhcommareaHostToJavaTransformer.java");

    }

    /**
     * Generate a java to host transformer case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenJavaToHostTransformer() throws Exception {
        genSourceAndCheck(CoxbGenWriter.JAVA_TO_HOST_XFORMER_VLC_TEMPLATE,
                "lsfileae", "Dfhcommarea",
                "DfhcommareaJavaToHostTransformer.java");
    }

    /**
     * An complex type containing a member with OCCURS 0 TO 1.
     * 
     * @throws Exception if generation fails
     */
    public void testVarar021Types() throws Exception {
        genSourceAndCheck(CoxbGenWriter.COMPLEX_VLC_TEMPLATE, "varar021",
                "Payload", "PayloadBinding.java");

    }

    /**
     * Generate a java to host transformer provider case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenTransformers() throws Exception {
        genSourceAndCheck(CoxbGenWriter.HOST_XFORMERS_VLC_TEMPLATE, "lsfileae",
                "Dfhcommarea", "DfhcommareaTransformers.java");

    }

    /**
     * Generate a java to host transformer provider case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenJvmqueryTransformers() throws Exception {
        genSourceAndCheck(CoxbGenWriter.HOST_XFORMERS_VLC_TEMPLATE, "jvmquery",
                "JvmQueryReply", "JvmQueryReplyTransformers.java");

    }

    /**
     * Test the host to XML template.
     * 
     * @throws Exception if generation fails
     */
    public void testHostToXmlTransformer() throws Exception {
        genSourceAndCheck(CoxbGenWriter.HOST_TO_XML_XFORMER_VLC_TEMPLATE,
                "alltypes", "Dfhcommarea",
                "DfhcommareaHostToXmlTransformer.java");
    }

    /**
     * Test the XML to host template.
     * 
     * @throws Exception if generation fails
     */
    public void testXmlToHostTransformer() throws Exception {
        genSourceAndCheck(CoxbGenWriter.XML_TO_HOST_XFORMER_VLC_TEMPLATE,
                "alltypes", "Dfhcommarea",
                "DfhcommareaXmlToHostTransformer.java");
    }

    /**
     * Test the XML transformers template.
     * 
     * @throws Exception if generation fails
     */
    public void testXmlTransformers() throws Exception {
        genSourceAndCheck(CoxbGenWriter.HOST_XML_XFORMERS_VLC_TEMPLATE,
                "alltypes", "Dfhcommarea", "DfhcommareaXmlTransformers.java");
    }

    /**
     * Generate a source from a template and check against reference.
     * 
     * @param templateName the velocity template under test
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     * @param fileName the target file name
     * @throws Exception usually if test not set properly
     */
    protected void genSourceAndCheck(final String templateName,
            final String schemaName, final String rootName,
            final String fileName) throws Exception {
        genSource(templateName, schemaName, rootName, fileName);
        check(schemaName, fileName);
    }

    /**
     * Generate a source from a template for a root complex type.
     * 
     * @param templateName the velocity template under test
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     * @param fileName the target file name
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final String templateName,
            final String schemaName, final String rootName,
            final String fileName) throws Exception {
        genSource(templateName, schemaName,
                getComplexBinding(schemaName, rootName), rootName, fileName);
    }

    /**
     * Generate a source from a template for any type.
     * 
     * @param templateName the velocity template under test
     * @param schemaName the originating XSD name
     * @param binding the type COBOL binding
     * @param rootName JAXB root class name
     * @param fileName the target file name
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final String templateName,
            final String schemaName, final ICobolBinding binding,
            final String rootName, final String fileName) throws Exception {

        genSource(templateName, schemaName, binding, rootName, fileName,
                createModel(schemaName));
    }

    /**
     * Generate a source from a template for any type.
     * 
     * @param templateName the velocity template under test
     * @param schemaName the originating XSD name
     * @param binding the type COBOL binding
     * @param rootName JAXB root class name
     * @param fileName the target file name
     * @param coxbContext parameter set
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final String templateName,
            final String schemaName, final ICobolBinding binding,
            final String rootName, final String fileName,
            CoxbGenModel coxbContext) throws Exception {

        File targetFolder = new File(GEN_SRC_DIR, coxbContext
                .getCoxbPackageName().replace(".", "/"));
        genSource(templateName, schemaName, binding, rootName, fileName,
                coxbContext, targetFolder);
    }

    /**
     * Generate a source from a template for any type.
     * 
     * @param templateName the velocity template under test
     * @param schemaName the originating XSD name
     * @param binding the type COBOL binding
     * @param rootName JAXB root class name
     * @param fileName the target file name
     * @param coxbContext parameter set
     * @param targetFolder where to store the generated content
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final String templateName,
            final String schemaName, final ICobolBinding binding,
            final String rootName, final String fileName,
            final CoxbGenModel coxbContext, final File targetFolder)
            throws Exception {

        getParameters().put("coxbContext", coxbContext);
        getParameters().put("binding-class-name", rootName + "Binding");

        FileUtils.forceMkdir(targetFolder);
        CodeGenUtil.processTemplate(BINDING_GENERATOR_NAME, templateName,
                "binding", binding, getParameters(),
                CodeGenUtil.getFile(targetFolder, fileName));
    }

}
