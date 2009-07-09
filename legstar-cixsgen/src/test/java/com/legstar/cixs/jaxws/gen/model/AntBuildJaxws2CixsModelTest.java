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
package com.legstar.cixs.jaxws.gen.model;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.Samples;
import com.legstar.cixs.jaxws.model.AntBuildJaxws2CixsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

/**
 * Test the generation of a generation ant script.
 *
 */
public class AntBuildJaxws2CixsModelTest extends AbstractTestTemplate {

    /**
     * Generate and test the ant script.
     * @throws Exception if test fails
     */
    public void testJaxws2CixsBuild() throws Exception {

        AntBuildJaxws2CixsModel antModel = new AntBuildJaxws2CixsModel();
        antModel.setProductLocation("/Users/Fady/sandbox/legstar-version");
        antModel.setProbeFile(new File("probe.file.tmp"));

        CixsJaxwsService cixsJaxwsService = Samples.getLsfileae();

        antModel.setCixsJaxwsService(cixsJaxwsService);
        antModel.setTargetBinDir(GEN_BIN_DIR);
        antModel.setTargetDistDir(GEN_DIST_DIR);
        antModel.setTargetSrcDir(GEN_SRC_DIR);
        antModel.setCoxbBinDir(GEN_BIN_DIR);
        antModel.setCustBinDir(GEN_BIN_DIR);
        antModel.setJaxbBinDir(GEN_BIN_DIR);
        antModel.setTargetAntDir(GEN_ANT_DIR);
        antModel.setTargetWarDir(GEN_WAR_DIR);
        antModel.setTargetWDDDir(GEN_WDD_DIR);
        antModel.setHostCharset("IBM01147");
        antModel.setWebServiceParameters(getDefaultWebServiceParameters(cixsJaxwsService));

        antModel.generateBuild(CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));
        String resStr = getSource(GEN_SRC_DIR, "test.txt");

        assertTrue(resStr.contains("<project basedir=\"/Users/Fady/sandbox/legstar-version\""
                + " default=\"signalSuccess\" name=\"generate-jaxws2cixs\">"));
        assertTrue(resStr.replace('\\', '/').contains("<pathelement location=\"target/src/gen/target/classes\"/>"));
        assertTrue(resStr.contains("<taskdef name=\"jaxws2cixsgen\""));
        assertTrue(resStr.contains("classname=\"com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator\""));
        assertTrue(resStr.replace('\\', '/').contains("<jaxws2cixsgen targetSrcDir=\"target/src/gen/java\""));
        assertTrue(resStr.replace('\\', '/').contains("targetWDDDir=\"target/src/gen/webapp\""));
        assertTrue(resStr.replace('\\', '/').contains("targetDistDir=\"target/src/gen/target\""));
        assertTrue(resStr.replace('\\', '/').contains("targetAntDir=\"target/src/gen/ant\""));
        assertTrue(resStr.replace('\\', '/').contains("targetWarDir=\"${env.CATALINA_BASE}/webapp\""));
        assertTrue(resStr.replace('\\', '/').contains("targetBinDir=\"target/src/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("jaxbBinDir=\"target/src/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("coxbBinDir=\"target/src/gen/target/classes\""));
        assertTrue(resStr.replace('\\', '/').contains("custBinDir=\"target/src/gen/target/classes\""));

        assertTrue(resStr.contains("<cixsJaxwsService name=\"lsfileae\""));
        assertTrue(resStr.contains("packageName=\"com.legstar.test.cixs.lsfileae\""));
        assertTrue(resStr.contains("<cixsOperation name=\"lsfileae\""));
        assertTrue(resStr.contains("cicsProgramName=\"LSFILEAE\""));
        assertTrue(resStr.contains("jaxbType=\"Dfhcommarea\""));
        assertTrue(resStr.contains("jaxbPackageName=\"com.legstar.test.coxb.lsfileae\""));
        assertTrue(resStr.replace('\\', '/').contains("<mkdir dir=\"target/src/gen/target/classes\"/>"));
        assertTrue(resStr.replace('\\', '/').contains("<javac srcdir=\"target/src/gen/java\""));

        assertTrue(resStr.contains("<webServiceParameters"));
        assertTrue(resStr.contains("wsdlTargetNamespace=\"http://cixs.test.legstar.com/lsfileae\""));
        assertTrue(resStr.contains("wsdlServiceName=\"lsfileaeService\""));
        assertTrue(resStr.contains("wsdlPortName=\"lsfileaePort\""));
    }

}
