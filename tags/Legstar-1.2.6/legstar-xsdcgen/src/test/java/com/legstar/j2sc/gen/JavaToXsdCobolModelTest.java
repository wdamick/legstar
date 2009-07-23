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
package com.legstar.j2sc.gen;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation of the ant script from the base model.
 *
 */
public class JavaToXsdCobolModelTest extends AbstractTest {

    /**
     * Generate and test ant script.
     * @throws Exception if generation fails
     */
    public void testBuilJ2sc() throws Exception {

        JavaToXsdCobolModel model = new JavaToXsdCobolModel();

        model.setProductLocation("/Users/Fady/sandbox/legstar-version");
        List < String > classNames = new ArrayList < String >();
        classNames.add("com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest");
        classNames.add("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply");
        model.setClassNames(classNames);
        List < String > pathElementLocations = new ArrayList < String >();
        pathElementLocations.add("/Users/pat/therat/cobol.jar");
        pathElementLocations.add("/Users/pat/thecat");
        model.setPathElementLocations(pathElementLocations);
        model.setJaxbPackageName("com.legstar.test.coxb.jvmquery");
        model.setJaxbTypeClassesSuffix("Typex");
        model.setNamespace("http://jvmquery.cases.test.xsdc.legstar.com/");
        model.setTargetDir(new File("schema"));
        model.setTargetXsdFileName("jvmquery.xsd");
        model.setProbeFile(new File("probe.file.tmp"));

        model.generateBuild(CodeGenUtil.getFile(GEN_DIR, "build.xml"));
        String result = getSource(GEN_DIR, "build.xml");

        assertTrue(result.contains("<project basedir=\"/Users/Fady/sandbox/legstar-version\""
                    + " default=\"signalSuccess\" name=\"generate-XSD\">"));
        assertTrue(result.contains("<pathelement location=\"/Users/pat/therat/cobol.jar\"/>"));
        assertTrue(result.contains("<pathelement location=\"/Users/pat/thecat\"/>"));
        assertTrue(result.contains("<echo message=\"Generating annotated XML schema jvmquery.xsd\" />"));
        assertTrue(result.contains("<mkdir dir=\"schema\"/>"));
        assertTrue(result.contains("namespace=\"http://jvmquery.cases.test.xsdc.legstar.com/\""));
        assertTrue(result.contains("jaxbPackageName=\"com.legstar.test.coxb.jvmquery\""));
        assertTrue(result.contains("jaxbTypeClassesSuffix=\"Typex\""));
        assertTrue(result.contains("targetDir=\"schema\""));
        assertTrue(result.contains("targetXsdFileName=\"jvmquery.xsd\""));
        assertTrue(result.contains("<rootClass name=\"com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest\"/>"));
        assertTrue(result.contains("<rootClass name=\"com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply\"/>"));
        assertTrue(result.contains("<delete file=\"probe.file.tmp\" quiet=\"true\"/>"));
    }

}
