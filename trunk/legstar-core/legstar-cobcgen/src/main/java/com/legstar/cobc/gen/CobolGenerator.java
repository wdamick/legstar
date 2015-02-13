/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobc.gen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.cobol.gen.CobolNameResolver;
import com.legstar.cobol.gen.CopybookGenerator;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.coxb.util.JAXBAnnotationException;
import com.legstar.coxb.util.JAXBElementDescriptor;

/**
 * This Ant task generates a Cobol data description source that can be used as a
 * copybook in a regular Cobol program. The Data description is extracted from
 * Cobol annotations within a JAXB class. Such Cobol-annotated JAXB classes are
 * produced by legstar-jaxbgen.
 */
public class CobolGenerator extends Task {

    /** Logger. */
    private final Log _log = LogFactory.getLog(CobolGenerator.class);

    /* ====================================================================== */
    /* = Properties section = */
    /* ====================================================================== */

    /** The package name used for JAXB classes. */
    private String jaxbPackageName;

    /** The JAXB type name. */
    private String jaxbTypeName;

    /** The Cobol data item name to use as a root. */
    private String cobolRootDataItemName;

    /** The target directory where generated Cobol will be created. */
    private File targetDir = null;

    /** The target generated Cobol file name. */
    private String targetCobolFileName;

    /** Where to start numbering Cobol data items. */
    private int firstCobolLevel;

    /** How much to increment Cobol data items moving from parent to child. */
    private int cobolLevelIncrement;

    /** If copybooks generated should include a comment header. */
    private boolean withHeader = true;

    /* ====================================================================== */
    /* = Local variables section = */
    /* ====================================================================== */

    /** Helper to suggest COBOL names from Java names. */
    private static CobolNameResolver cobolNameResolver;

    /**
     * The ant execute method. Generates a new Cobol data description source.
     */
    public void execute() {
        if (_log.isDebugEnabled()) {
            _log.debug("Cobol source generation started");
        }
        checkInput();
        String outPath = targetDir.getPath() + File.separator
                + targetCobolFileName;
        BufferedWriter writer = null;
        try {
            String code = generate(jaxbPackageName, jaxbTypeName,
                    cobolRootDataItemName, firstCobolLevel,
                    cobolLevelIncrement, withHeader);
            writer = new BufferedWriter(new FileWriter(new File(outPath)));
            writer.write(code);
        } catch (IOException e) {
            throw new BuildException(e);
        } catch (CobolGenerationException e) {
            throw new BuildException(e);
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    _log.error(e);
                }
            }
        }
        if (_log.isDebugEnabled()) {
            _log.debug("Cobol source generation ended");
        }
    }

    /**
     * Checks that properties set are valid.
     */
    private void checkInput() {

        if (_log.isDebugEnabled()) {
            _log.debug("checkInput started");
            _log.debug("   Source JAXB package    = " + jaxbPackageName);
            _log.debug("   Source JAXB type name  = " + jaxbTypeName);
            _log.debug("   Root data item name    = " + cobolRootDataItemName);
            _log.debug("   First data item level  = " + firstCobolLevel);
            _log.debug("   Level increment        = " + cobolLevelIncrement);
            _log.debug("   Target directory       = " + targetDir);
            _log.debug("   Target Cobol file name = " + targetCobolFileName);
            _log.debug("   Include header comment = " + withHeader);
        }
        /* Check that we have a valid JAXB type name. */
        if (jaxbTypeName == null || jaxbTypeName.length() == 0) {
            throw (new BuildException("You must provide a JAXB type name"));
        }

        /* Check that we have a valid target directory. */
        if (targetDir == null) {
            throw (new BuildException("You must provide a target directory"));
        }
        if (!targetDir.exists()) {
            throw (new BuildException("Directory " + targetDir
                    + " does not exist"));
        }
        if (!targetDir.isDirectory() || !targetDir.canWrite()) {
            throw (new BuildException(targetDir
                    + " is not a directory or is not writable"));
        }

        /* Check levels */
        if (firstCobolLevel < 0 || firstCobolLevel > 49) {
            throw (new BuildException(firstCobolLevel
                    + " is not a valid COBOL level number"));
        }
        if (cobolLevelIncrement < 0 || cobolLevelIncrement > 49) {
            throw (new BuildException(cobolLevelIncrement
                    + " is not a valid COBOL level increment"));
        }

        /* Set valid default level related values */
        if (firstCobolLevel == 0) {
            firstCobolLevel = 1;
        }
        if (cobolLevelIncrement == 0) {
            cobolLevelIncrement = 1;
        }

        /* Set a valid root cobol data item */
        if (cobolRootDataItemName == null
                || cobolRootDataItemName.length() == 0) {
            cobolRootDataItemName = jaxbTypeName;
        }

        /* Set a valid target generated Cobol file name */
        if (targetCobolFileName == null || targetCobolFileName.length() == 0) {
            targetCobolFileName = jaxbTypeName + ".cbl";
        }

        if (_log.isDebugEnabled()) {
            _log.debug("checkInput ended");
        }
    }

    /**
     * Using the <code>cobcgen</code> utility, this will use reflection to
     * instantiate a Jaxb object corresponding to a structure received and then
     * generate COBOL data description code using the COBOL annotations in the
     * jaxb class.
     * 
     * @param jaxbPackageName the JAXB classes package name
     * @param jaxbType the JAXB class
     * @param cobolRootDataItemName the COBOL structure root data item name
     * @param firstCobolLevel where to start numbering Cobol data items
     * @param cobolLevelIncrement how much to increment Cobol data items moving
     *            from parent to child
     * @return data description COBOL source code for the structure
     * @throws CobolGenerationException if code generation fails
     */
    public static String generate(final String jaxbPackageName,
            final String jaxbType, final String cobolRootDataItemName,
            final int firstCobolLevel, final int cobolLevelIncrement)
            throws CobolGenerationException {
        return generate(jaxbPackageName, jaxbType, cobolRootDataItemName,
                firstCobolLevel, cobolLevelIncrement, true);
    }

    /**
     * Using the <code>cobcgen</code> utility, this will use reflection to
     * instantiate a Jaxb object corresponding to a structure received and then
     * generate COBOL data description code using the COBOL annotations in the
     * jaxb class.
     * 
     * @param jaxbPackageName the JAXB classes package name
     * @param jaxbType the JAXB class
     * @param cobolRootDataItemName the COBOL structure root data item name
     * @param firstCobolLevel where to start numbering Cobol data items
     * @param cobolLevelIncrement how much to increment Cobol data items moving
     *            from parent to child
     * @param withHeader true if copybook should start with a header comment
     * @return data description COBOL source code for the structure
     * @throws CobolGenerationException if code generation fails
     */
    public static String generate(final String jaxbPackageName,
            final String jaxbType, final String cobolRootDataItemName,
            final int firstCobolLevel, final int cobolLevelIncrement,
            final boolean withHeader) throws CobolGenerationException {
        try {
            JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
                    jaxbPackageName, jaxbType);
            Object objectFactory = elementDescriptor.getObjectFactory();
            Class < ? > clazz = elementDescriptor.getJaxbClass();
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    objectFactory, clazz);
            String cobolRootName = cobolRootDataItemName;
            if (cobolRootName == null || cobolRootName.length() == 0) {
                cobolRootName = getCobolNameResolver().getName(jaxbType);
            }
            ccem.setCobolName(cobolRootName);
            CobolGenVisitor cev = new CobolGenVisitor(firstCobolLevel,
                    cobolLevelIncrement);
            ccem.accept(cev);

            return CopybookGenerator
                    .generate(cev.getRootDataItem(), withHeader);
        } catch (ReflectBindingException e) {
            throw new CobolGenerationException(e);
        } catch (HostException e) {
            throw new CobolGenerationException(e);
        } catch (JAXBAnnotationException e) {
            throw new CobolGenerationException(e);
        }
    }

    /**
     * @return the current name resolver or a new one if none existed before
     */
    private static CobolNameResolver getCobolNameResolver() {
        if (cobolNameResolver == null) {
            cobolNameResolver = new CobolNameResolver();
        }
        return cobolNameResolver;
    }

    /**
     * @return the package name used for JAXB classes
     */
    public String getJaxbPackageName() {
        return jaxbPackageName;
    }

    /**
     * @param jaxbPackageName the JAXB classes package name to set
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        this.jaxbPackageName = jaxbPackageName;
    }

    /**
     * @return Returns the JAXB type name.
     */
    public String getJaxbTypeName() {
        return jaxbTypeName;
    }

    /**
     * @param jaxbTypeName The JAXB type name to set.
     */
    public void setJaxbTypeName(final String jaxbTypeName) {
        this.jaxbTypeName = jaxbTypeName;
    }

    /**
     * @return the current target directory
     */
    public File getTargetDir() {
        return targetDir;
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setTargetDir(final File targetDir) {
        this.targetDir = targetDir;
    }

    /**
     * @return the target generated Cobol file name
     */
    public String getTargetCobolFileName() {
        return targetCobolFileName;
    }

    /**
     * @param targetCobolFileName the target generated Cobol file name to set
     */
    public void setTargetCobolFileName(final String targetCobolFileName) {
        this.targetCobolFileName = targetCobolFileName;
    }

    /**
     * @return the Cobol Root Data Item Name
     */
    public String getCobolRootDataItemName() {
        return cobolRootDataItemName;
    }

    /**
     * @param cobolRootDataItemName the Cobol Root Data Item Name to set
     */
    public void setCobolRootDataItemName(final String cobolRootDataItemName) {
        this.cobolRootDataItemName = cobolRootDataItemName;
    }

    /**
     * @return where to start numbering Cobol data items
     */
    public int getFirstCobolLevel() {
        return firstCobolLevel;
    }

    /**
     * @param firstCobolLevel where to start numbering Cobol data items
     */
    public void setFirstCobolLevel(final int firstCobolLevel) {
        this.firstCobolLevel = firstCobolLevel;
    }

    /**
     * @return how much to increment Cobol data items moving from parent to
     *         child
     */
    public int getCobolLevelIncrement() {
        return cobolLevelIncrement;
    }

    /**
     * @param cobolLevelIncrement how much to increment Cobol data items moving
     *            from parent to child
     */
    public void setCobolLevelIncrement(final int cobolLevelIncrement) {
        this.cobolLevelIncrement = cobolLevelIncrement;
    }

    /**
     * @return true if generated copybooks should include a comment header
     */
    public boolean isWithHeader() {
        return withHeader;
    }

    /**
     * @param withHeader set to true if generated copybooks should include a
     *            comment header
     */
    public void setWithHeader(boolean withHeader) {
        this.withHeader = withHeader;
    }

}
