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
package com.legstar.coxb.gen;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectHelper;

import com.legstar.codegen.CodeGenUtil;

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
    public void setUp() {
        super.setUp();
        CodeGenUtil.checkDirectory(GEN_ANT_DIR, true);
        FileUtils.deleteQuietly(GEN_SRC_DIR);
        CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
        CodeGenUtil.checkDirectory(GEN_BIN_DIR, true);
        _model = new CoxbGenModel();
    }

    /**
     * Generate an ant script capable of producing the binding artifacts using
     * the VLC template. Sumit the script and check results.
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
                .contains("<echo message=\"Generating binding classes for [class1, class2]\" />"));
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
     * Execute an ant script.
     * 
     * @param buildFile the ant script
     * @throws Exception if ant script execution fails
     */
    protected void runAnt(final File buildFile) throws Exception {
        final Project project = new Project();
        // project.addBuildListener(new CommonsLoggingListener(_log));
        project.setCoreLoader(this.getClass().getClassLoader());
        project.init();
        ProjectHelper helper = ProjectHelper.getProjectHelper();
        project.addReference("ant.projectHelper", helper);
        helper.parse(project, buildFile);
        Vector < String > targets = new Vector < String >();
        targets.addElement(project.getDefaultTarget());
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
}
