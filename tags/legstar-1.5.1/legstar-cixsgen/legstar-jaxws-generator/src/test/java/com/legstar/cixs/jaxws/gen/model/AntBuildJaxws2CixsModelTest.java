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

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** {@inheritDoc} */
    public void setUp() {
        super.setUp();
        setCreateReferences(CREATE_REFERENCES);
    }

    /**
     * Generate and test the ant script.
     * 
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
        antModel.setNoPackageInfo(true);

        String resFileName = "build.xml";
        antModel.generateBuild(CodeGenUtil.getFile(GEN_DIR, resFileName));

        String refFileName = getRefFileName(resFileName);
        check(new File(REF_RES_DIR, refFileName),
                new File(GEN_DIR, resFileName));

    }

}
