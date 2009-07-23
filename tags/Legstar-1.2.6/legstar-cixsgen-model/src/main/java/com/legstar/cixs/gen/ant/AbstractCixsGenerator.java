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
package com.legstar.cixs.gen.ant;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;
import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.gen.CoxbHelper;

/**
 * This class groups methods that are common to all generators using mapping
 * from Mainframe programs to local operations.
 */
public abstract class AbstractCixsGenerator extends Task {

    /** Groups all parameters needed for generation. */
    private AbstractAntBuildCixsModel mAntModel;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Constructor.
     * @param antModel an instance of a generation model
     */
    public AbstractCixsGenerator(final AbstractAntBuildCixsModel antModel) {
        mAntModel = antModel;
    }

    /** @{inheritDoc}*/
    @Override
    public final void init() {
        _log.info("Initializing velocity engine for "
                + mAntModel.getGeneratorName());
        try {
            CodeGenUtil.initVelocity();
        } catch (Exception e) {
            throw new BuildException(e.getMessage());
        }
    }

    /**
     * Check that enough input parameters are set and then
     * generate the requested artifacts.
     * 
     * */
    @Override
    public final void execute() {
        _log.info("Generating artifacts for "
                + ((getCixsService() == null) ? "null"
                        : getCixsService().getName()));
        long start = System.currentTimeMillis();

        try {
            checkInput();
            completeModel();
            generate();

        } catch (CodeGenMakeException e) {
            _log.error("Generator " + getGeneratorName() + " failure ", e);
            throw new BuildException(e);
        }

        long end = System.currentTimeMillis();
        _log.info("Generation success for " + getCixsService().getName());
        _log.info("Duration = " + (end - start) + " ms");
    }

    /**
     * Check that input values are valid.
     * @throws CodeGenMakeException if input is invalid
     */
    private void checkInput() throws CodeGenMakeException {
        try {
            CodeGenUtil.checkDirectory(
                    getJaxbBinDir(), false, "JaxbBinDir");

            if (getCixsService() == null) {
                throw new CodeGenMakeException(
                "You must specify a service description");
            }
            String serviceName = getCixsService().getName();
            if (serviceName == null || serviceName.length() == 0) {
                throw new CodeGenMakeException(
                "You must provide a service name");
            }

            CodeGenUtil.checkCharset(getHostCharset());
        } catch (IllegalArgumentException e) {
            throw new CodeGenMakeException(e);
        }

        /* Give subclasses a chance to perform extra validation.*/
        checkExtendedInput();
    }

    /**
     * Prepare a set of common parameters used by most generation 
     * targets and then call subclass generation.
     * @throws CodeGenMakeException if generation fails
     */
    private void generate() throws CodeGenMakeException {
        Map < String, Object > parameters = new HashMap < String, Object >();
        CodeGenHelper helper = new CodeGenHelper();
        parameters.put("helper", helper);
        CoxbHelper coxbHelper = new CoxbHelper();
        parameters.put("coxbHelper", coxbHelper);

        /* These parameters are primarily useful for the ant build template */
        parameters.put("targetSrcDir", getTargetSrcDir());
        parameters.put("targetBinDir", getTargetBinDir());
        parameters.put("targetAntDir", getTargetAntDir());
        parameters.put("targetDistDir", getTargetDistDir());
        parameters.put("jaxbBinDir", getJaxbBinDir());
        parameters.put("coxbBinDir", getCoxbBinDir());
        parameters.put("custBinDir", getCustBinDir());

        /* Subclasses will implement the real generation.*/
        generate(parameters);

    }

    /**
     * Allow more validations for descendants.
     * @throws CodeGenMakeException if validation fails
     */
    public abstract void checkExtendedInput() throws CodeGenMakeException; 

    /**
     * Create all artifacts for service.
     * @param parameters a predefined set of parameters useful for generation
     * @throws CodeGenMakeException if generation fails
     */
    public abstract void generate(
            Map < String, Object > parameters) throws CodeGenMakeException;


    /**
     * @return this generator name
     */
    public abstract String getGeneratorName();

    /**
     * Generate default values where they are missing in the model. This
     * will reduce the amount of code in the velocity templates.
     */
    protected void completeModel() {
        for (CixsOperation operation : getCixsService().getCixsOperations()) {
            if (operation.getPackageName() == null 
                    || operation.getPackageName().length() == 0) {
                operation.setPackageName(getCixsService().getPackageName());
            }
        }
    }

    /**
     * Create a file by applying a velocity template.
     * @param generatorName the generator name
     * @param templateName which velocity template to apply
     * @param modelName name by which velocity templates reference the model
     * @param model the object holding all generation parameters
     * @param parameters miscellaneous help parameters
     * @param dir where to store the generated file
     * @param fileName name of generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateFile(
            final String generatorName,
            final String templateName,
            final String modelName,
            final Object model,
            final Map < String, Object > parameters,
            final File dir,
            final String fileName)
    throws CodeGenMakeException {

        File targetFile = CodeGenUtil.getFile(dir, fileName);
        CodeGenUtil.processTemplate(
                generatorName,
                templateName,
                modelName, model,
                parameters,
                targetFile);
    }

    /**
     * @return the service description 
     */
    public final AbstractCixsService getCixsService() {
        return mAntModel.getCixsService();
    }

    /**
     * @param cixsService the service to set
     */
    public final void setCixsService(
            final AbstractCixsService cixsService) {
        mAntModel.setCixsService(cixsService);
    }

    /**
     * @param cixsService the service to set
     */
    public final void add(final AbstractCixsService cixsService) {
        setCixsService(cixsService);
    }

    /**
     * @param cixsService the service to set
     */
    public final void addCixsService(
            final AbstractCixsService cixsService) {
        setCixsService(cixsService);
    }

    /**
     * @return the target source directory
     */
    public final File getTargetSrcDir() {
        return mAntModel.getTargetSrcDir();
    }

    /**
     * @param targetSrcDir the target source directory to set
     */
    public final void setTargetSrcDir(final File targetSrcDir) {
        mAntModel.setTargetSrcDir(targetSrcDir);
    }
    /**
     * @return custom binaries location
     */
    public final File getCustBinDir() {
        return mAntModel.getCustBinDir();
    }

    /**
     * @param custBinDir the custom binaries location to set
     */
    public final void setCustBinDir(final File custBinDir) {
        mAntModel.setCustBinDir(custBinDir);
    }

    /**
     * @return the deployment location for jaxws war files
     */
    public final File getTargetDistDir() {
        return mAntModel.getTargetDistDir();
    }

    /**
     * @param targetDistDir the distribution location for artifacts such as jars and wars to set
     */
    public final void setTargetDistDir(final File targetDistDir) {
        mAntModel.setTargetDistDir(targetDistDir);
    }

    /**
     * @return the Service binaries
     */
    public final File getTargetBinDir() {
        return mAntModel.getTargetBinDir();
    }

    /**
     * @param targetBinDir the Service binaries to set
     */
    public final void setTargetBinDir(final File targetBinDir) {
        mAntModel.setTargetBinDir(targetBinDir);
    }

    /**
     * @return the jaxb binaries location
     */
    public final File getJaxbBinDir() {
        return mAntModel.getJaxbBinDir();
    }

    /**
     * @param jaxbBinDir the jaxb binaries location to set
     */
    public final void setJaxbBinDir(final File jaxbBinDir) {
        mAntModel.setJaxbBinDir(jaxbBinDir);
    }

    /**
     * @return the coxb binaries location
     */
    public final File getCoxbBinDir() {
        return mAntModel.getCoxbBinDir();
    }

    /**
     * @param coxbBinDir the coxb binaries location to set
     */
    public final void setCoxbBinDir(final File coxbBinDir) {
        mAntModel.setCoxbBinDir(coxbBinDir);
    }

    /**
     * @return the location for ant deployment script
     */
    public final File getTargetAntDir() {
        return mAntModel.getTargetAntDir();
    }

    /**
     * @param targetAntDir the location for ant deployment script to set
     */
    public final void setTargetAntDir(final File targetAntDir) {
        mAntModel.setTargetAntDir(targetAntDir);
    }

    /**
     * @return the model representing all generation parameters
     */
    public AbstractAntBuildCixsModel getAntModel() {
        return mAntModel;
    }

    /**
     * @return the host character set
     */
    public final String getHostCharset() {
        return mAntModel.getHostCharset();
    }

    /**
     * @param hostCharset the host character set to set
     */
    public final void setHostCharset(final String hostCharset) {
        mAntModel.setHostCharset(hostCharset);
    }

}
