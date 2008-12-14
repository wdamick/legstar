/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.jaxws.gen;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import com.legstar.cixs.gen.ant.AbstractCixsGenerator;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.jaxws.model.AntBuildCixs2JaxwsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;

/**
 * This Ant task creates the various Jaxws artifacts needed to implement
 * a servlet proxy that acts as an adapter for a Web Service so that a
 * mainframe program can call the target Web Service without any knowledge
 * of SOAP.
 * The task also generates a sample COBOL CICS program that demonstrates
 * how to call the proxy servlet. 
 */
public class Cixs2JaxwsGenerator extends AbstractCixsGenerator {

    /** This generator name. */
    public static final String CIXS_TO_JAXWS_GENERATOR_NAME =
        "LegStar Mainframe to Jaxws generator";

    /** Velocity template for war ant build generation. */
    public static final String SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE =
        "vlc/c2j-service-ant-build-war-xml.vm";

    /** Velocity template for web descriptor generation. */
    public static final String SERVICE_WEB_XML_VLC_TEMPLATE =
        "vlc/c2j-service-web-xml.vm";

    /** Velocity template for COBOL client generation. */
    public static final String OPERATION_COBOL_CICS_CLIENT_VLC_TEMPLATE =
        "vlc/c2j-operation-cobol-cics-client.vm";

    /** The service model name is it appears in templates. */
    public static final String SERVICE_MODEL_NAME = "model";

    /**
     * Constructor.
     */
    public Cixs2JaxwsGenerator() {
        super(new AntBuildCixs2JaxwsModel());
    }

    /** {@inheritDoc}*/
    public void checkExtendedInput() throws CodeGenMakeException {
        try {
            CodeGenUtil.checkDirectory(
                    getTargetAntDir(), true, "TargetAntDir");
            CodeGenUtil.checkDirectory(
                    getTargetWDDDir(), true, "TargetWDDDir");
            CodeGenUtil.checkDirectory(
                    getTargetCobolDir(), true, "TargetCobolDir");
            /* Check that we are provided with valid locations to
             * reference.*/
            if (getTargetWarDir() == null) {
                throw (new IllegalArgumentException(
                "TargetWarDir: No directory name was specified"));
            }

            /* Check that we have a URI to expose to mainframe programs */
            String serviceURI = getCixsJaxwsService().getServiceURI();
            CodeGenUtil.checkHttpURI(serviceURI);

            /* Check that we have a target Web Service WSDL URL */
            String wsdlUrl = getCixsJaxwsService().getWsdlUrl();
            if (wsdlUrl == null || wsdlUrl.length() == 0) {
                throw new CodeGenMakeException(
                "You must specify a target Web Service WSDL URL");
            }
            new URI(wsdlUrl);

            /* Check that we have CICS program names mapped to operations */
            for (CixsOperation operation : getCixsOperations()) {
                String cicsProgramName = operation.getCicsProgramName();
                if (cicsProgramName == null || cicsProgramName.length() == 0) {
                    throw new CodeGenMakeException(
                    "Operation must specify a CICS program name");
                }
            }
        } catch (IllegalArgumentException e) {
            throw new CodeGenMakeException(e);
        } catch (URISyntaxException e) {
            throw new CodeGenMakeException(e);
        }
    }

    /** {@inheritDoc}*/
    public void generate(final Map < String, Object > parameters)
    throws CodeGenMakeException {

        parameters.put("targetWarDir", getTargetWarDir());
        parameters.put("targetWDDDir", getTargetWDDDir());
        parameters.put("hostCharset", getHostCharset());
        parameters.put("structHelper", new StructuresGenerator());

        /* Determine target files locations */
        File serviceWebFilesDir = getTargetWDDDir();
        CodeGenUtil.checkDirectory(serviceWebFilesDir, true);
        File serviceAntFilesDir = getTargetAntDir();
        CodeGenUtil.checkDirectory(serviceAntFilesDir, true);
        File serviceCobolCicsClientDir = getTargetCobolDir();
        CodeGenUtil.checkDirectory(serviceCobolCicsClientDir, true);

        generateWebXml(
                getCixsJaxwsService(), parameters, serviceWebFilesDir);
        generateAntBuildWar(
                getCixsJaxwsService(), parameters, serviceAntFilesDir);

        for (CixsOperation operation : getCixsService().getCixsOperations()) {
            parameters.put("cixsOperation", operation);
            generateCobolCicsClient(
                    getCixsJaxwsService(), operation, parameters,
                    serviceCobolCicsClientDir);
        }

    }

    /**
     * Create the Ant Build for a War file generation.
     * @param service the service description
     * @param parameters miscellaneous help parameters
     * @param serviceAntFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateAntBuildWar(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceAntFilesDir)
    throws CodeGenMakeException {
        generateFile(CIXS_TO_JAXWS_GENERATOR_NAME,
                SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceAntFilesDir,
        "build.xml");
    }

    /**
     * Create the Jaxws Web Xml file.
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceWebFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateWebXml(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceWebFilesDir)
    throws CodeGenMakeException {
        generateFile(CIXS_TO_JAXWS_GENERATOR_NAME,
                SERVICE_WEB_XML_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceWebFilesDir,
        "web.xml");
    }

    /**
     * Create a COBOl CICS Client program to use for testing.
     * @param service the Jaxws service description
     * @param operation the operation for which a program is to be generated
     * @param parameters the set of parameters to pass to template engine
     * @param cobolFilesDir location where COBOL code should be generated
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateCobolCicsClient(
            final CixsJaxwsService service,
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File cobolFilesDir)
    throws CodeGenMakeException {
        generateFile(CIXS_TO_JAXWS_GENERATOR_NAME,
                OPERATION_COBOL_CICS_CLIENT_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                cobolFilesDir,
                operation.getCicsProgramName() + ".cbl");
    }

    /**
     * @return the Target location for web deployment descriptors
     */
    public final File getTargetWDDDir() {
        return getModel().getTargetWDDDir();
    }

    /**
     * @param targetWDDDir the Target location for web deployment descriptors to
     *  set
     */
    public final void setTargetWDDDir(final File targetWDDDir) {
        getModel().setTargetWDDDir(targetWDDDir);
    }

    /**
     * @return the deployment location for jaxws war files
     */
    public final File getTargetWarDir() {
        return getModel().getTargetWarDir();
    }

    /**
     * @param targetWarDir the deployment location for jaxws war files to set
     */
    public final void setTargetWarDir(final File targetWarDir) {
        getModel().setTargetWarDir(targetWarDir);
    }

    /**
     * @return the directory where COBOL files will be created
     */
    public final File getTargetCobolDir() {
        return getModel().getTargetCobolDir();
    }

    /**
     * @param targetCobolDir the directory where COBOL files will be created
     *  to set
     */
    public final void setTargetCobolDir(final File targetCobolDir) {
        getModel().setTargetCobolDir(targetCobolDir);
    }

    /**
     * @return the service description
     */
    public final CixsJaxwsService getCixsJaxwsService() {
        return (CixsJaxwsService) getCixsService();
    }

    /**
     * @param cixsJaxwsService the service description to set
     */
    public final void setCixsJaxwsService(
            final CixsJaxwsService cixsJaxwsService) {
        setCixsService(cixsJaxwsService);
    }

    /**
     * @param cixsJaxwsService the Jaxws service to set
     */
    public final void add(final CixsJaxwsService cixsJaxwsService) {
        setCixsService(cixsJaxwsService);
    }

    /**
     * @param cixsJaxwsService the Jaxws service to set
     */
    public final void addCixsJaxwsService(
            final CixsJaxwsService cixsJaxwsService) {
        setCixsService(cixsJaxwsService);
    }

    /**
     * {@inheritDoc}
     * @see com.legstar.cixs.gen.ant.AbstractCixsGenerator#getModel()
     */
    public AntBuildCixs2JaxwsModel getModel() {
        return (AntBuildCixs2JaxwsModel) super.getModel();
    }

    /**
     * Convenience method to get the inner mapped operations.
     * @return the operations list
     */
    public List < CixsOperation > getCixsOperations() {
        return getCixsJaxwsService().getCixsOperations();
    }

    /** {@inheritDoc}*/
    public String getGeneratorName() {
        return CIXS_TO_JAXWS_GENERATOR_NAME;
    }

}
