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
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.options.HttpTransportParameters;
import com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator;
import com.legstar.cixs.jaxws.gen.StructuresGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;

/**
 * Test the sample COBOL WMQ Client generation.
 * 
 */
public class SampleCobolClientTemplatesTest extends AbstractTestTemplate {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** {@inheritDoc} */
    public void setUp() {
        super.setUp();
        setCreateReferences(CREATE_REFERENCES);
    }

    /**
     * Generate the c2ws sample COBOL client and test it.
     * 
     * @throws Exception if generation fails
     */
    public void testCobolCicsC2wsJvmquery() throws Exception {

        CixsJaxwsService service = Samples.getJvmqueryWs();
        getParameters()
                .put("cixsOperation", service.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        HttpTransportParameters httpTransportParameters = getDefaultHttpParameters(service);
        httpTransportParameters.setUserId("alice");
        httpTransportParameters.setPassword("inwonderland");
        httpTransportParameters.add(getParameters());

        processAndCheck(
                service,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_DFHWBCLI_CLIENT_VLC_TEMPLATE);
    }

    /**
     * Generate the c2ws sample COBOL client and test it.
     * 
     * @throws Exception if generation fails
     */
    public void testCobolCicsC2wsCultureinfo() throws Exception {

        CixsJaxwsService service = Samples.getCultureInfo();
        getParameters()
                .put("cixsOperation", service.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        addHttpTransportParameters(service, getParameters());
        processAndCheck(
                service,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_LSHTTAPI_CLIENT_VLC_TEMPLATE);

    }

    /**
     * Generate the c2ws sample COBOL client and test it.
     * 
     * @throws Exception if generation fails
     */
    public void testCobolCicsC2wsMSNSearch() throws Exception {

        CixsJaxwsService service = Samples.getMSNSearch();
        getParameters()
                .put("cixsOperation", service.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        addHttpTransportParameters(service, getParameters());
        processAndCheck(
                service,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_LSHTTAPI_CLIENT_VLC_TEMPLATE);

    }

    /**
     * Generate the DFHWBCLI sample COBOL client and test it.
     * 
     * @throws Exception if generation fails
     */
    public void testCobolCicsDfhwbcliClientGeneration() throws Exception {

        CixsJaxwsService service = Samples.getJvmqueryWs();
        getParameters()
                .put("cixsOperation", service.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        addHttpTransportParameters(service, getParameters());
        processAndCheck(
                service,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_DFHWBCLI_CLIENT_VLC_TEMPLATE);

    }

    /**
     * Generate the WEB API sample COBOL client and test it.
     * 
     * @throws Exception if generation fails
     */
    public void testCobolCicsWebapiClientGeneration() throws Exception {

        CixsJaxwsService service = Samples.getJvmqueryWs();
        getParameters()
                .put("cixsOperation", service.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        addHttpTransportParameters(service, getParameters());
        processAndCheck(
                service,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_WEBAPI_CLIENT_VLC_TEMPLATE);
    }

    /**
     * Generate the WMQ sample COBOL client and test it.
     * 
     * @throws Exception if generation fails
     */
    public void testCobolCicsWmqClientGeneration() throws Exception {

        CixsJaxwsService service = Samples.getJvmqueryWs();
        getParameters()
                .put("cixsOperation", service.getCixsOperations().get(0));
        getParameters().put("structHelper", new StructuresGenerator());
        addWmqTransportParameters(service, getParameters());
        processAndCheck(
                service,
                Cixs2JaxwsGenerator.OPERATION_COBOL_CICS_WMQ_CLIENT_VLC_TEMPLATE);
    }

    /**
     * Apply a velocity template and produce a source.
     * 
     * @param service model to use
     * @param templateName the velocity template to apply
     * @throws Exception if something goes wrong
     */
    protected void processAndCheck(final CixsJaxwsService service,
            final String templateName) throws Exception {
        super.processTemplate(service,
                Cixs2JaxwsGenerator.CIXS_TO_JAXWS_GENERATOR_NAME, templateName,
                GEN_COBOL_DIR, service.getCixsOperations().get(0)
                        .getCicsProgramName()
                        + ".cbl");
        check(service, service.getCixsOperations().get(0));
    }

    /**
     * Check generated artifact against the reference.
     * 
     * @param operation the model's operation
     * @throws Exception if can't get reference
     */
    protected void check(final CixsJaxwsService model,
            final CixsOperation operation) throws Exception {
        String resFileName = operation.getCicsProgramName() + ".cbl";
        String refFileName = getRefFileName(resFileName);
        check(new File(REF_RES_DIR, refFileName), new File(GEN_COBOL_DIR,
                resFileName));
    }
}
