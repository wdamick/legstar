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
package com.legstar.cixs.jaxws.gen;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.gen.ant.AbstractCixsGenerator;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.jaxws.model.AbstractAntBuildCixsJaxwsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;

/**
 * This Ant task creates the various Jaxws artifacts needed to implement
 * adapters and proxies.
 */
public abstract class AbstractCixsJaxwsGenerator extends AbstractCixsGenerator {

    /** This generator name. */
    public static final String JAXWS_GENERATOR_NAME =
        "LegStar Mainframe JAX-WS generator";

    /** The service model name is it appears in templates. */
    public static final String SERVICE_MODEL_NAME = "model";

    /** For tracing purposes, this file contains various version related attributes. */
    private static final String VERSION_FILE =
        "/com/legstar/cixs/jaxws/legstar-jaxws-generator-version.properties";

    /** This are the version related properties. */
    private Properties mVersionProperties;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Constructor.
     * @param model an instance of a generation model
     */
    public AbstractCixsJaxwsGenerator(final AbstractAntBuildCixsJaxwsModel model) {
        super(model);
        InputStream inStream = AbstractCixsJaxwsGenerator.class.getResourceAsStream(VERSION_FILE);
        mVersionProperties = new Properties();
        if (inStream != null) {
            try {
                mVersionProperties.load(inStream);
            } catch (IOException e) {
                _log.warn("Unable to read version file " + VERSION_FILE);
            } finally {
                try { inStream.close(); } catch (Throwable e) { e.printStackTrace(); }
            }
        }
        if (_log.isDebugEnabled()) {
            _log.debug(getVersion());
        }
    }

    /**
     * @return the title and version details of this module
     */
    public String getVersion() {
        return mVersionProperties.getProperty("Implementation-Title")
        + " version: " + mVersionProperties.getProperty("Implementation-Version");
    }

    /** {@inheritDoc}*/
    public void checkExtendedInput() throws CodeGenMakeException {
        try {
            CodeGenUtil.checkDirectory(
                    getTargetAntDir(), true, "TargetAntDir");
            CodeGenUtil.checkDirectory(
                    getTargetWDDDir(), true, "TargetWDDDir");
            CodeGenUtil.checkDirectory(
                    getTargetDistDir(), true, "TargetDistDir");

            /* Check that we are provided with valid locations to
             * reference.*/
            if (getTargetWarDir() == null) {
                throw (new IllegalArgumentException(
                "TargetWarDir: No directory name was specified"));
            }

            /* Check that we have at least one operation. */
            if (getCixsOperations().size() == 0) {
                throw new CodeGenMakeException(
                        "No operation was specified");
            }
            
            /* Check that we have CICS program names mapped to operations */
            for (CixsOperation operation : getCixsOperations()) {
                String cicsProgramName = operation.getCicsProgramName();
                if (cicsProgramName == null || cicsProgramName.length() == 0) {
                    throw new CodeGenMakeException(
                    "Operation must specify a CICS program name");
                }
            }

            checkExtendedExtendedInput();

        } catch (IllegalArgumentException e) {
            throw new CodeGenMakeException(e);
        }
    }

    /**
     * Give the inheriting generators a chance to add more controls.
     * @throws CodeGenMakeException if control fails
     */
    public abstract void checkExtendedExtendedInput() throws CodeGenMakeException;

    /** {@inheritDoc}*/
    public void generate(final Map < String, Object > parameters)
    throws CodeGenMakeException {

        addParameters(parameters);

        generateExtended(parameters);
    }
    
    /**
     * Create more artifacts for a Jboss ESB service.
     * @param parameters a predefined set of parameters useful for generation
     * @throws CodeGenMakeException if generation fails
     */
    public abstract void generateExtended(
            final Map < String, Object > parameters) throws CodeGenMakeException;

    /**
     * Add common parameters expected by templates to come from a parameters map.
     * @param parameters a parameters map to which parameters must be added
     */
    private void addParameters(final Map < String, Object > parameters) {
        parameters.put("targetWarDir", getTargetWarDir());
        parameters.put("targetDistDir", getTargetDistDir());
        parameters.put("targetWDDDir", getTargetWDDDir());
        parameters.put("hostCharset", getHostCharset());
        
        addExtendedParameters(parameters);
    }
    
    
    /**
     * Add common parameters expected by templates to come from a parameters map.
     * @param parameters a parameters map to which parameters must be added
     */
    public abstract void addExtendedParameters(final Map < String, Object > parameters);

    /**
     * @return the Target location for web deployment descriptors
     */
    public final File getTargetWDDDir() {
        return getAntModel().getTargetWDDDir();
    }

    /**
     * @param targetWDDDir the Target location for web deployment descriptors to
     *  set
     */
    public final void setTargetWDDDir(final File targetWDDDir) {
        getAntModel().setTargetWDDDir(targetWDDDir);
    }

    /**
     * @return the deployment location for jaxws war files
     */
    public final File getTargetWarDir() {
        return getAntModel().getTargetWarDir();
    }

    /**
     * @param targetWarDir the deployment location for jaxws war files to set
     */
    public final void setTargetWarDir(final File targetWarDir) {
        getAntModel().setTargetWarDir(targetWarDir);
    }

    /**
     * @return the service description
     */
    public final CixsJaxwsService getCixsJaxwsService() {
        return getAntModel().getCixsJaxwsService();
    }

    /**
     * @param cixsJaxwsService the service description to set
     */
    public final void setCixsJaxwsService(
            final CixsJaxwsService cixsJaxwsService) {
        getAntModel().setCixsJaxwsService(cixsJaxwsService);
    }

    /**
     * @param cixsJaxwsService the Jaxws service to set
     */
    public final void add(final CixsJaxwsService cixsJaxwsService) {
        getAntModel().setCixsJaxwsService(cixsJaxwsService);
    }

    /**
     * @param cixsJaxwsService the Jaxws service to set
     */
    public final void addCixsJaxwsService(
            final CixsJaxwsService cixsJaxwsService) {
        getAntModel().setCixsJaxwsService(cixsJaxwsService);
    }

    /**
     * {@inheritDoc}
     * @see com.legstar.cixs.gen.ant.AbstractCixsGenerator#getAntModel()
     */
    public AbstractAntBuildCixsJaxwsModel getAntModel() {
        return (AbstractAntBuildCixsJaxwsModel) super.getAntModel();
    }

    /**
     * Convenience method to get the inner mapped operations.
     * @return the operations list
     */
    public List < CixsOperation > getCixsOperations() {
        return getCixsJaxwsService().getCixsOperations();
    }

}
