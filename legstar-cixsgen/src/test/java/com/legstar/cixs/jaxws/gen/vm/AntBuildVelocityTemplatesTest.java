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
package com.legstar.cixs.jaxws.gen.vm;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;


/**
 * Test generation of ant build files. 
 *
 */
public class AntBuildVelocityTemplatesTest extends AbstractTestTemplate {

   
    
    /** @{inheritDoc}*/
    @Override
    public void setUp() {
        super.setUp();
        getParameters().put("targetDistDir", "/legstar-cixsgen-cases/target");
        getParameters().put("jaxbBinDir", "/legstar-jaxbgen-cases/target/classes");
        getParameters().put("coxbBinDir", "/legstar-coxbgen-cases/target/classes");
        getParameters().put("targetBinDir", "/legstar-cixsgen-cases/target/classes");
        getParameters().put("custBinDir", "/legstar-cixsgen-cust-cases/target/classes");

    }
    /**
     * Lsfileae case.
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {

        CixsJaxwsService jaxwsComponent = Samples.getLsfileae();
        initWebServiceParameters(jaxwsComponent);

        File componentAntFilesDir =
            new File(GEN_ANT_DIR, jaxwsComponent.getName());
        CodeGenUtil.checkDirectory(componentAntFilesDir, true);
        String fileName = Jaxws2CixsGenerator.generateAntBuildJar(
                jaxwsComponent, getParameters(), componentAntFilesDir);
        String resStr = getSource(componentAntFilesDir, fileName);

        assertTrue(resStr.contains("<jar destfile=\"/legstar-cixsgen-cases/target/cixs-lsfileae.jar\">"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-cixsgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/cixs/lsfileae/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-jaxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileae/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-coxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileae/bind/*.class\"/>"));
    }

    /**
     * Lsfileal case.
     * @throws Exception if test fails
     */
    public void testLsfileal() throws Exception {

        CixsJaxwsService jaxwsComponent = Samples.getLsfileal();
        initWebServiceParameters(jaxwsComponent);

        File componentAntFilesDir =
            new File(GEN_ANT_DIR, jaxwsComponent.getName());
        CodeGenUtil.checkDirectory(componentAntFilesDir, true);
        String fileName = Jaxws2CixsGenerator.generateAntBuildJar(
                jaxwsComponent, getParameters(), componentAntFilesDir);
        String resStr = getSource(componentAntFilesDir, fileName);

        assertTrue(resStr.contains("<jar destfile=\"/legstar-cixsgen-cases/target/cixs-lsfileal.jar\">"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-cixsgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/cixs/lsfileal/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-jaxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileal/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-coxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileal/bind/*.class\"/>"));
    }

    /**
     * Lsfileac case.
     * @throws Exception if test fails
     */
    public void testLsfileac() throws Exception {

        CixsJaxwsService jaxwsComponent = Samples.getLsfileac();
        initWebServiceParameters(jaxwsComponent);

        File componentAntFilesDir =
            new File(GEN_ANT_DIR, jaxwsComponent.getName());
        CodeGenUtil.checkDirectory(componentAntFilesDir, true);
        String fileName = Jaxws2CixsGenerator.generateAntBuildJar(
                jaxwsComponent, getParameters(), componentAntFilesDir);
        String resStr = getSource(componentAntFilesDir, fileName);

        assertTrue(resStr.contains("<jar destfile=\"/legstar-cixsgen-cases/target/cixs-lsfileac.jar\">"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-cixsgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/cixs/lsfileac/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-jaxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileac/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-coxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileac/bind/*.class\"/>"));
    }

    /**
     * Lsfileax case.
     * @throws Exception if test fails
     */
    public void testLsfileax() throws Exception {

        CixsJaxwsService jaxwsComponent = Samples.getLsfileax();
        initWebServiceParameters(jaxwsComponent);

        File componentAntFilesDir =
            new File(GEN_ANT_DIR, jaxwsComponent.getName());
        CodeGenUtil.checkDirectory(componentAntFilesDir, true);
        String fileName = Jaxws2CixsGenerator.generateAntBuildJar(
                jaxwsComponent, getParameters(), componentAntFilesDir);
        String resStr = getSource(componentAntFilesDir, fileName);

        assertTrue(resStr.contains("<jar destfile=\"/legstar-cixsgen-cases/target/cixs-lsfileax.jar\">"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-cixsgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/cixs/lsfileax/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-jaxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileae/*.class\"/>"));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileac/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-coxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileae/bind/*.class\"/>"));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/lsfileac/bind/*.class\"/>"));
    }

    /**
     * Dplarcht case.
     * @throws Exception if test fails
     */
    public void testDplarcht() throws Exception {

        CixsJaxwsService jaxwsComponent = Samples.getDplarcht();
        initWebServiceParameters(jaxwsComponent);

        File componentAntFilesDir =
            new File(GEN_ANT_DIR, jaxwsComponent.getName());
        CodeGenUtil.checkDirectory(componentAntFilesDir, true);
        String fileName = Jaxws2CixsGenerator.generateAntBuildJar(
                jaxwsComponent, getParameters(), componentAntFilesDir);
        String resStr = getSource(componentAntFilesDir, fileName);

        assertTrue(resStr.contains("<jar destfile=\"/legstar-cixsgen-cases/target/cixs-dplarcht.jar\">"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-cixsgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/cixs/dplarcht/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-jaxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/dplarcht/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-coxbgen-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/test/coxb/dplarcht/bind/*.class\"/>"));
        assertTrue(resStr.contains("<fileset dir=\"/legstar-cixsgen-cust-cases/target/classes\""));
        assertTrue(resStr.contains("includes=\"com/legstar/coxb/cust/dplarcht/*.class\"/>"));
    }
}
