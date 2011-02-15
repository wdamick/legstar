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
package com.legstar.coxb.gen;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Vector;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectHelper;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.jaxb.gen.JaxbGenModel;

/**
 * Test the generation model.
 * 
 */
public class CoxbGenModelTest extends AbstractTestTemplate {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** The parameters set. */
    private CoxbGenModel _model;

    /** @{inheritDoc */
    public void setUp() throws Exception {
        super.setUp();
        FileUtils.forceMkdir(GEN_SRC_DIR);
        FileUtils.cleanDirectory(GEN_SRC_DIR);
        FileUtils.forceMkdir(GEN_ANT_DIR);
        FileUtils.cleanDirectory(GEN_ANT_DIR);
        FileUtils.forceMkdir(GEN_BIN_DIR);
        FileUtils.cleanDirectory(GEN_BIN_DIR);
        _model = new CoxbGenModel();
    }

    /**
     * Generate an ant script capable of producing the binding artifacts using
     * the VLC template. Submit the script and check results.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testBuildCoxb() throws Exception {

        List < String > jaxbRootClassNames = new ArrayList < String >();
        jaxbRootClassNames.add("Dfhcommarea");

        _model.setProductLocation("../../../..");
        _model.setProbeFile(new File("probe.file.tmp"));
        _model.setJaxbSrcDir(GEN_SRC_DIR);
        _model.setJaxbBinDir(GEN_BIN_DIR);
        _model.setXsdFile(getSchemaFromFolder("lsfileae"));
        _model.setCoxbSrcDir(GEN_SRC_DIR);
        _model.setCoxbBinDir(GEN_BIN_DIR);
        _model.setJaxbRootClassNames(jaxbRootClassNames);
        _model.setJaxbPackageName("com.legstar.test.coxb.lsfileae");

        runAnt(genAntScriptAsFile());

        File bindingFile = new File(GEN_SRC_DIR,
                "com/legstar/test/coxb/lsfileae/bind/"
                        + "DfhcommareaBinding.java");
        String bindingSrce = FileUtils.readFileToString(bindingFile);
        if (_log.isDebugEnabled()) {
            _log.debug(bindingSrce);
        }
        assertTrue(bindingSrce.contains("public class DfhcommareaBinding"));

    }

    /**
     * Test the COXB customization parameters.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testCOXBCustomization() throws Exception {

        List < String > jaxbRootClassNames = new ArrayList < String >();
        jaxbRootClassNames.add("class1");
        jaxbRootClassNames.add("class2");

        _model.setProductLocation("/Users/Fady/sandbox/legstar-1.2.0");
        _model.setXsdFile(new File("myXsd.xsd"));
        _model.setJaxbPackageName("com.legstar.test");
        _model.setCoxbSrcDir(new File("coxb/src"));
        _model.setCoxbBinDir(new File("coxb/bin"));
        _model.setJaxbRootClassNames(jaxbRootClassNames);

        String resStr = genAntScriptAsString();

        assertTrue(resStr
                .contains("<echo message=\"Generating Transformers for [class1, class2]\" level=\"info\"/>"));
        assertTrue(resStr.contains("xsdFile=\"myXsd.xsd\""));
        assertTrue(resStr.contains("targetDir=\"coxb\\src\""));
        assertTrue(resStr.contains("<jaxbRootClass name=\"class1\"/>"));
        assertTrue(resStr.contains("<jaxbRootClass name=\"class2\"/>"));
        assertTrue(resStr.contains("<javac srcdir=\"coxb\\src\""));
        assertTrue(resStr.contains("destdir=\"coxb\\bin\""));
    }

    /**
     * Test the JAXB customization parameters.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testJAXBCustomization() throws Exception {
        _model.setGenerateIsSetMethod(false);
        _model.setSerializableUid(122145458787L);
        _model.setElementNamePrefix("elementNamePrefix");
        _model.setElementNameSuffix("elementNameSuffix");
        _model.setTypeNamePrefix("typeNamePrefix");
        _model.setTypeNameSuffix("typeNameSuffix");

        String resStr = genAntScriptAsString();

        assertTrue(resStr.contains("generateIsSetMethod=\"false\""));
        assertTrue(resStr.contains("serializableUid=\"122145458787\""));
        assertTrue(resStr.contains("typeNamePrefix=\"typeNamePrefix\""));
        assertTrue(resStr.contains("typeNameSuffix=\"typeNameSuffix\""));
        assertTrue(resStr.contains("elementNamePrefix=\"elementNamePrefix\""));
        assertTrue(resStr.contains("elementNameSuffix=\"elementNameSuffix\""));

    }

    /**
     * Test the choice strategy parameters.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testChoiceStrategyParameter() throws Exception {
        _model.setGenerateIsSetMethod(false);
        _model.addUnmarshalChoiceStrategy(new UnmarshalChoiceStrategy(
                "ITEMA:a.StrategyA"));
        _model.addUnmarshalChoiceStrategy(new UnmarshalChoiceStrategy(
                "ITEMB:b.StrategyB"));

        String resStr = genAntScriptAsString();

        assertTrue(resStr
                .contains("<unmarshalChoiceStrategy redefinedCobolItem=\"ITEMA\""
                        + " unmarshalChoiceStrategyClassName=\"a.StrategyA\"/>"));
        assertTrue(resStr
                .contains("<unmarshalChoiceStrategy redefinedCobolItem=\"ITEMB\""
                        + " unmarshalChoiceStrategyClassName=\"b.StrategyB\"/>"));

    }

    /**
     * Generate and submit the ant script with choice strategy parameter.
     * 
     * @throws Exception if ant cannot be generated
     */
    public void testBuildCoxbWithStrategy() throws Exception {

        List < String > jaxbRootClassNames = new ArrayList < String >();
        jaxbRootClassNames.add("Dfhcommarea");

        _model.setProductLocation("../../../..");
        _model.setProbeFile(new File("probe.file.tmp"));
        _model.setJaxbSrcDir(GEN_SRC_DIR);
        _model.setJaxbBinDir(GEN_BIN_DIR);
        _model.setXsdFile(getSchemaFromFolder("redsimpt"));
        _model.setCoxbSrcDir(GEN_SRC_DIR);
        _model.setCoxbBinDir(GEN_BIN_DIR);
        _model.setJaxbRootClassNames(jaxbRootClassNames);
        _model.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
        _model.addUnmarshalChoiceStrategy(new UnmarshalChoiceStrategy(
                "C-DEFINITION-1:a.StrategyA"));

        runAnt(genAntScriptAsFile());

        File bindingFile = new File(GEN_SRC_DIR,
                "com/legstar/test/coxb/redsimpt/bind/"
                        + "DfhcommareaBinding.java");
        String bindingSrce = FileUtils.readFileToString(bindingFile);
        if (_log.isDebugEnabled()) {
            _log.debug(bindingSrce);
        }
        assertTrue(bindingSrce
                .contains("_cDefinition1Choice.setUnmarshalChoiceStrategyClassName("));
        assertTrue(bindingSrce.contains("\"a.StrategyA\""));

    }

    /**
     * Test the serialization and construction using a properties file.
     * 
     * @throws Exception if test fails
     */
    public void testPropertySerialization() throws Exception {
        CoxbGenModel model = new CoxbGenModel();
        Properties props = model.toProperties();
        assertEquals(null, props.get(JaxbGenModel.JAXB_XSD_LOCATION));
        assertEquals("true",
                props.get(JaxbGenModel.JAXB_XJB_ISGENERATEISSETMETHOD));
        assertEquals("1", props.get(JaxbGenModel.JAXB_XJB_SERIALIZABLE_ID));
        assertEquals(null, props.get(JaxbGenModel.JAXB_XJB_ELEMENTNAME_PREFIX));
        assertEquals(null, props.get(JaxbGenModel.JAXB_XJB_ELEMENTNAME_SUFFIX));
        assertEquals(null, props.get(JaxbGenModel.JAXB_XJB_TYPENAME_PREFIX));
        assertEquals(null, props.get(JaxbGenModel.JAXB_XJB_TYPENAME_SUFFIX));

        assertEquals(null, props.get(CoxbGenModel.COXB_PACKAGENAME));
        assertEquals(null,
                props.get(CoxbGenModel.COXB_JAXB_ALTERNATIVEPACKAGENAME));
        assertEquals(null,
                props.get(CoxbGenModel.COXB_JAXB_ALTERNATIVEFACTORYNAME));
        assertEquals("true", props.get(CoxbGenModel.COXB_ISXMLTRANSFORMERS));
        assertEquals("false", props.get(CoxbGenModel.COXB_ISJSONTRANSFORMERS));
        assertEquals(null, props.get(CoxbGenModel.COXB_XSDFILE));
        assertEquals(null, props.get(CoxbGenModel.COXB_JAXBROOTCLASSNAMES));
        assertEquals(null, props.get(CoxbGenModel.COXB_JAXBSRCDIR));
        assertEquals(null, props.get(CoxbGenModel.COXB_JAXBBINDIR));
        assertEquals(null, props.get(CoxbGenModel.COXB_COXBSRCDIR));
        assertEquals(null, props.get(CoxbGenModel.COXB_COXBBINDIR));
        assertEquals(null,
                props.get(CoxbGenModel.COXB_UNMARSHAL_CHOICE_STRATEGIES));

        assertEquals("{isCompileTransformers=false,"
                + " isXmlTransformers=true," + " serializableID=1,"
                + " internalBindings=true," + " isJsonTransformers=false,"
                + " generateIsSetMethod=true}", model.toString());

        model = new CoxbGenModel(props);
        assertEquals(null, model.getJaxbPackageName());
        JaxbGenModel xjbModel = new JaxbGenModel();
        xjbModel.setXsdLocation("xsdLocation");
        xjbModel.setSerializableUid(265L);
        xjbModel.setGenerateIsSetMethod(false);
        xjbModel.setElementNamePrefix("elementNamePrefix");
        xjbModel.setElementNameSuffix("elementNameSuffix");
        xjbModel.setTypeNamePrefix("typeNamePrefix");
        xjbModel.setTypeNameSuffix("typeNameSuffix");
        props.putAll(xjbModel.toProperties());

        model = new CoxbGenModel(props);
        assertEquals("xsdLocation", model.getJaxbGenModel().getXsdLocation());
        assertEquals(265L, model.getSerializableUid());
        assertEquals(false, model.isGenerateIsSetMethod());
        assertEquals("elementNamePrefix", model.getElementNamePrefix());
        assertEquals("elementNameSuffix", model.getElementNameSuffix());
        assertEquals("typeNamePrefix", model.getTypeNamePrefix());
        assertEquals("typeNameSuffix", model.getTypeNameSuffix());

        props.put(CoxbGenModel.COXB_PACKAGENAME, "coxb.package.name");
        model = new CoxbGenModel(props);
        assertEquals("coxb.package.name", model.getCoxbPackageName());

        props.put(CoxbGenModel.COXB_JAXB_ALTERNATIVEPACKAGENAME,
                "jaxb.alt.package.name");
        props.put(CoxbGenModel.COXB_JAXB_ALTERNATIVEFACTORYNAME,
                "jaxb.alt.package.name.AltFactory");
        model = new CoxbGenModel(props);
        assertEquals("jaxb.alt.package.name", model.getAlternativePackageName());
        assertEquals("jaxb.alt.package.name.AltFactory",
                model.getAlternativeFactoryName());

        props.put(CoxbGenModel.COXB_ISXMLTRANSFORMERS, "true");
        props.put(CoxbGenModel.COXB_ISJSONTRANSFORMERS, "true");
        model = new CoxbGenModel(props);
        assertTrue(model.isXmlTransformers());
        assertTrue(model.isJsonTransformers());

        props.put(CoxbGenModel.COXB_JAXBROOTCLASSNAMES + "_0", "FirstClass");
        props.put(CoxbGenModel.COXB_JAXBROOTCLASSNAMES + "_1", "SecondClass");
        model = new CoxbGenModel(props);
        assertEquals("[FirstClass, SecondClass]", model.getJaxbRootClassNames()
                .toString());

        props.put(CoxbGenModel.COXB_XSDFILE, "xsdfile");
        props.put(CoxbGenModel.COXB_JAXBSRCDIR, "jaxbsrcdir");
        props.put(CoxbGenModel.COXB_JAXBBINDIR, "jaxbbindir");
        props.put(CoxbGenModel.COXB_COXBSRCDIR, "coxbsrcdir");
        props.put(CoxbGenModel.COXB_COXBBINDIR, "coxbbindir");
        model = new CoxbGenModel(props);
        assertEquals("xsdfile", model.getXsdFile().getPath());
        assertEquals("jaxbsrcdir", model.getJaxbSrcDir().getPath());
        assertEquals("jaxbbindir", model.getJaxbBinDir().getPath());
        assertEquals("coxbsrcdir", model.getCoxbSrcDir().getPath());
        assertEquals("coxbbindir", model.getCoxbBinDir().getPath());
        props.put(CoxbGenModel.COXB_UNMARSHAL_CHOICE_STRATEGIES + "_0",
                "ITEMA:some.Strategy");
        model = new CoxbGenModel(props);
        assertEquals("ITEMA:some.Strategy", model
                .getUnmarshalChoiceStrategies().get(0).toString());

        props = model.toProperties();
        assertEquals("coxb.package.name",
                props.getProperty(CoxbGenModel.COXB_PACKAGENAME));
        assertEquals(
                "jaxb.alt.package.name",
                props.getProperty(CoxbGenModel.COXB_JAXB_ALTERNATIVEPACKAGENAME));
        assertEquals(
                "jaxb.alt.package.name.AltFactory",
                props.getProperty(CoxbGenModel.COXB_JAXB_ALTERNATIVEFACTORYNAME));
        assertEquals("true",
                props.getProperty(CoxbGenModel.COXB_ISXMLTRANSFORMERS));
        assertEquals("true",
                props.getProperty(CoxbGenModel.COXB_ISJSONTRANSFORMERS));
        assertEquals((new File("xsdfile")).getCanonicalPath(),
                props.getProperty(CoxbGenModel.COXB_XSDFILE));
        assertEquals("FirstClass",
                props.getProperty(CoxbGenModel.COXB_JAXBROOTCLASSNAMES + "_0"));
        assertEquals("SecondClass",
                props.getProperty(CoxbGenModel.COXB_JAXBROOTCLASSNAMES + "_1"));
        assertEquals((new File("jaxbsrcdir")).getCanonicalPath(),
                props.getProperty(CoxbGenModel.COXB_JAXBSRCDIR));
        assertEquals((new File("jaxbbindir")).getCanonicalPath(),
                props.getProperty(CoxbGenModel.COXB_JAXBBINDIR));
        assertEquals((new File("coxbsrcdir")).getCanonicalPath(),
                props.getProperty(CoxbGenModel.COXB_COXBSRCDIR));
        assertEquals((new File("coxbbindir")).getCanonicalPath(),
                props.getProperty(CoxbGenModel.COXB_COXBBINDIR));

        assertEquals("xsdLocation",
                props.getProperty(JaxbGenModel.JAXB_XSD_LOCATION));
        assertEquals("false",
                props.getProperty(JaxbGenModel.JAXB_XJB_ISGENERATEISSETMETHOD));
        assertEquals("265",
                props.getProperty(JaxbGenModel.JAXB_XJB_SERIALIZABLE_ID));
        assertEquals("elementNamePrefix",
                props.getProperty(JaxbGenModel.JAXB_XJB_ELEMENTNAME_PREFIX));
        assertEquals("elementNameSuffix",
                props.getProperty(JaxbGenModel.JAXB_XJB_ELEMENTNAME_SUFFIX));
        assertEquals("typeNamePrefix",
                props.getProperty(JaxbGenModel.JAXB_XJB_TYPENAME_PREFIX));
        assertEquals("typeNameSuffix",
                props.getProperty(JaxbGenModel.JAXB_XJB_TYPENAME_SUFFIX));

        assertEquals(
                "ITEMA:some.Strategy",
                props.getProperty(CoxbGenModel.COXB_UNMARSHAL_CHOICE_STRATEGIES
                        + "_0"));
    }

    /**
     * Execute an ant script.
     * 
     * @param buildFile the ant script
     * @throws Exception if ant script execution fails
     */
    protected void runAnt(final File buildFile) throws Exception {
        final Project project = new Project();
        project.setCoreLoader(this.getClass().getClassLoader());
        project.init();
        ProjectHelper helper = ProjectHelper.getProjectHelper();
        project.addReference("ant.projectHelper", helper);
        helper.parse(project, buildFile);
        Vector < String > targets = new Vector < String >();
        targets.addElement(project.getDefaultTarget());
        project.setBaseDir(new File("."));
        project.executeTargets(targets);
    }

    /**
     * Generates an ant script from a VLC template.
     * 
     * @return the script as a string
     * @throws Exception if generation fails
     */
    protected File genAntScriptAsFile() throws Exception {
        File resultFile = CodeGenUtil.getFile(GEN_ANT_DIR, "build.xml");
        _model.generateBuild(resultFile);
        return resultFile;
    }

    /**
     * Generates an ant script from a VLC template.
     * 
     * @return the script as a string
     * @throws Exception if generation fails
     */
    protected String genAntScriptAsString() throws Exception {
        File resultFile = genAntScriptAsFile();
        return FileUtils.readFileToString(resultFile, "UTF-8");
    }

    /**
     * Test package to includes.
     * 
     * @throws Exception if generation fails
     */
    public void testToIncludes() throws Exception {
        _model.setJaxbPackageName("pac");
        assertEquals("pac/**", _model.getJaxbClassIncludes());
        _model.setJaxbPackageName("com.pac");
        assertEquals("com/pac/**", _model.getJaxbClassIncludes());
    }
}
