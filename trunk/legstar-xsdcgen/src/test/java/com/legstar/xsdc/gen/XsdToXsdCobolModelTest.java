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
package com.legstar.xsdc.gen;

import java.io.File;
import java.net.URI;

import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation of the ant script from the base model.
 * 
 */
public class XsdToXsdCobolModelTest extends AbstractTest {

    /**
     * Generate and test ant script.
     * 
     * @throws Exception if generation fails
     */
    public void testBuilX2sc() throws Exception {

        XsdToXsdCobolModel model = new XsdToXsdCobolModel();

        model.setProductLocation("/Users/Fady/sandbox/legstar-version");
        model.setInputXsdUri(new URI("http://some.location/wsdl"));
        model.setNamespace("http://jvmquery.cases.test.xsdc.legstar.com/");
        model.setTargetDir(new File("schema"));
        model.setTargetXsdFileName("jvmquery.xsd");
        model.setProbeFile(new File("probe.file.tmp"));

        model.generateBuild(CodeGenUtil.getFile(GEN_DIR, "build.xml"));
        String result = getSource(GEN_DIR, "build.xml");

        assertTrue(result
                .contains("<project basedir=\"/Users/Fady/sandbox/legstar-version\""
                        + " default=\"signalSuccess\" name=\"generate-XSD\">"));
        assertTrue(result
                .contains("<echo message=\"Generating annotated XML schema jvmquery.xsd\" />"));
        assertTrue(result.contains("<mkdir dir=\"schema\"/>"));
        assertTrue(result.contains("inputXsdUri=\"http://some.location/wsdl\""));
        assertTrue(result
                .contains("namespace=\"http://jvmquery.cases.test.xsdc.legstar.com/\""));
        assertTrue(result.contains("targetDir=\"schema\""));
        assertTrue(result.contains("targetXsdFileName=\"jvmquery.xsd\""));
        assertTrue(result
                .contains("<delete file=\"probe.file.tmp\" quiet=\"true\"/>"));
    }

}
