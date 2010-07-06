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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractAntBuildModel;

/**
 * A model usable for Binding classes generation.
 * Groups all the data needed to generate a set of binding classes and
 * an intermediary set of jaxb classes from a COBOL-annotated XML schema.
 * Gathers all parameters that are needed during the lifetime of the generation
 * process. This allows more parameters to be added without too much impact on
 * other classes.
 *
 */
public class CoxbGenModel extends AbstractAntBuildModel {

    /** The JAXB/COXB annotated XML schema file. */
    private File mXsdFile;

    /** The package name used for JAXB classes. */
    private String mJaxbPackageName;

    /** The location where JAXB classes sources live.
     * This is not strictly needed for binding generation but is useful
     * when this model is also used for JAXB classes generation. */
    private File mJaxbSrcDir;

    /** The location where JAXB compiled classes live. */
    private File mJaxbBinDir;

    /** The location where JAXB external binding files (XJBs) are located. */
    private File mJaxbXjcBindingDir;

    /** A set of Jaxb root class names to generated binding classes for. */
    private List < String > mJaxbRootClassNames;

    /** The target package name for generated binding classes. */
    private String mCoxbPackageName;

    /** The target directory where source files will be created. */
    private File mCoxbSrcDir;

    /** The target directory where binary files will be created. */
    private File mCoxbBinDir;

    /** An optional runtime alternative to the Jaxb package name used at
     * generation time. */
    private String mAlternativePackageName;

    /** At runtime, if a alternativePackageName is specified, this alternative
     * factory can be used rather than the JAXB one. */
    private String mAlternativeFactoryName;

    /** The additional package level for generated binding classes. */
    private static final String COXB_PACKAGE_SUFFIX = "bind";

    /** This generator name. */
    public static final String COXB_GENERATOR_NAME =
        "LegStar Binding generator";

    /** This velocity template. */
    public static final String COXB_VELOCITY_MACRO_NAME =
        "vlc/build-coxb-xml.vm";

    /** Logger. */
    private final Log _log = LogFactory.getLog(CoxbGenModel.class);

    /**
     * Provides a complete trace of parameters values.
     */
    public void traceContext() {
        _log.debug("   JAXB classes sources location     ="
                + " " + getJaxbSrcDir());
        _log.debug("   JAXB classes binaries location    ="
                + " " + getJaxbBinDir());
        _log.debug("   JAXB Package name                 ="
                + " " + getJaxbPackageName());
        if (getJaxbRootClassNames() != null) {
            for (String jaxbRootClassName : getJaxbRootClassNames()) {
                _log.debug("   JAXB root class name              ="
                        + " " + jaxbRootClassName);
            }
        }
        _log.debug("   COBOL annotated XML schema file   ="
                + " " + getXsdFile());
        _log.debug("   Binding classes source location   ="
                + " " + getCoxbSrcDir());
        _log.debug("   Binding classes binaries location ="
                + " " + getCoxbBinDir());
        _log.debug("   Binding classes Package name      ="
                + " " + getCoxbPackageName());
        _log.debug("   Alternative package name          ="
                + " " + getAlternativePackageName());
        _log.debug("   Alternative factory name          ="
                + " " + getAlternativeFactoryName());
    }

    /**
     * Creates an ant build script file ready for binding generation.
     * @param scriptFile the script file that must be created
     * @throws CodeGenMakeException if generation fails
     */
    public void generateBuild(
            final File scriptFile) throws CodeGenMakeException {
        super.generateBuild(
                COXB_GENERATOR_NAME, COXB_VELOCITY_MACRO_NAME, scriptFile);
    }

    /**
     * Adds a jaxb root class name to generate a binding class for.
     * @param className The JAXB root class name to set.
     */
    public void addJaxbRootClassName(
            final String className) {
        if (mJaxbRootClassNames == null) {
            mJaxbRootClassNames = new ArrayList < String >();
        }
        if (!mJaxbRootClassNames.contains(className)) {
            mJaxbRootClassNames.add(className);
        }
    }

    /**
     * @return the XML schema file
     */
    public File getXsdFile() {
        return mXsdFile;
    }

    /**
     * @param xsdFile the XML schema file to set
     */
    public void setXsdFile(
            final File xsdFile) {
        mXsdFile = xsdFile;
    }

    /**
     * @return the current target directory
     */
    public File getCoxbSrcDir() {
        return mCoxbSrcDir;
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setCoxbSrcDir(final File targetDir) {
        mCoxbSrcDir = targetDir;
    }

    /**
     * @return the location where JAXB classes live
     */
    public File getJaxbBinDir() {
        return mJaxbBinDir;
    }

    /**
     * @param jaxbBinDir the JAXB location to set
     */
    public void setJaxbBinDir(final File jaxbBinDir) {
        mJaxbBinDir = jaxbBinDir;
    }

    /**
     * @return the package name used for JAXB classes
     */
    public String getJaxbPackageName() {
        return mJaxbPackageName;
    }

    /**
     * @param jaxbPackageName the JAXB classes package name to set
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        mJaxbPackageName = jaxbPackageName;
    }

    /**
     * @return the package name for generated binding classes
     */
    public String getCoxbPackageName() {
        if (mCoxbPackageName == null 
                || mCoxbPackageName.length() == 0) {
            if (mJaxbPackageName == null
                    || mJaxbPackageName.length() == 0) {
                return mCoxbPackageName;
            }
            return mJaxbPackageName + '.' + COXB_PACKAGE_SUFFIX;
        }
        return mCoxbPackageName;
    }

    /**
     * @param coxbPackageName package name for generated binding classes to set
     */
    public void setCoxbPackageName(final String coxbPackageName) {
        mCoxbPackageName = coxbPackageName;
    }

    /**
     * @return the optional runtime alternative to the Jaxb package name used at
     * generation time
     */
    public String getAlternativePackageName() {
        return mAlternativePackageName;
    }

    /**
     * @param alternativePackageName the optional runtime alternative to the
     * Jaxb package name used at generation time
     */
    public void setAlternativePackageName(
            final String alternativePackageName) {
        mAlternativePackageName = alternativePackageName;
    }

    /**
     * @return the alternate factory to used rather than the JAXB one.
     */
    public String getAlternativeFactoryName() {
        return mAlternativeFactoryName;
    }

    /**
     * @param targetFactoryName the alternate factory to used rather than the
     * JAXB one
     */
    public void setAlternativeFactoryName(
            final String targetFactoryName) {
        mAlternativeFactoryName = targetFactoryName;
    }

    /**
     * This is not strictly needed for binding generation but is useful
     * when this model is also used for JAXB classes generation.
     * @return the location where JAXB classes sources live
     */
    public File getJaxbSrcDir() {
        return mJaxbSrcDir;
    }

    /**
     * @param jaxbSrcDir the location where JAXB classes sources live to set
     */
    public void setJaxbSrcDir(final File jaxbSrcDir) {
        mJaxbSrcDir = jaxbSrcDir;
    }

    /**
     * @return the location where JAXB external binding files (XJBs) are located
     */
    public File getJaxbXjcBindingDir() {
        return mJaxbXjcBindingDir;
    }

    /**
     * @param jaxbXjcBindingDir the location where JAXB external binding files
     *  (XJBs) are located to set
     */
    public void setJaxbXjcBindingDir(
            final File jaxbXjcBindingDir) {
        mJaxbXjcBindingDir = jaxbXjcBindingDir;
    }

    /**
     * @return the set of Jaxb root class names to generated binding classes for
     */
    public List < String > getJaxbRootClassNames() {
        return mJaxbRootClassNames;
    }

    /**
     * @param jaxbRootClassNames the set of Jaxb root class names to generated
     *  binding classes for to set
     */
    public void setJaxbRootClassNames(
            final List < String > jaxbRootClassNames) {
        mJaxbRootClassNames = jaxbRootClassNames;
    }

    /**
     * @return the target directory where binary files will be created
     */
    public File getCoxbBinDir() {
        return mCoxbBinDir;
    }

    /**
     * @param coxbBinDir the target directory where binary files will be created
     *  to set
     */
    public void setCoxbBinDir(final File coxbBinDir) {
        mCoxbBinDir = coxbBinDir;
    }


}
