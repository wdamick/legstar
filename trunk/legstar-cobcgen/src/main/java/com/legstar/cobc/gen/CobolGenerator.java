/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.cobc.gen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;
import com.legstar.xsdc.gen.CobolNameResolver;
import com.legstar.xsdc.gen.CobolNameResolverException;

/**
 * This Ant task generates a Cobol data description source that can be used as 
 * a copybook in a regular Cobol program. The Data description is extracted from
 * Cobol annotations within a JAXB class. Such Cobol-annotated JAXB classes are
 * produced by legstar-jaxbgen.
 */
public class CobolGenerator extends Task  {

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CobolGenerator.class);

	/* ====================================================================== */
	/* = Properties section                                                 = */
	/* ====================================================================== */

	/** The package name used for JAXB classes. */
	private String mJaxbPackageName;
	
	/** The JAXB type name. */
	private String mJaxbTypeName;
	
	/** The Cobol data item name to use as a root. */
	private String mCobolRootDataItemName;
	
	/** The target directory where generated Cobol will be created. */
	private File mTargetDir;
	
	/** The target generated Cobol file name. */
	private String mTargetCobolFileName;
	
	/** Where to start numbering Cobol data items. */
	private int mFirstCobolLevel;
	
	/** How much to increment Cobol data items moving from parent to child. */
	private int mCobolLevelIncrement;
	
	/* ====================================================================== */
	/* = Local variables section                                            = */
	/* ====================================================================== */

    /** Helper to suggest COBOL names from Java names. */
    private static CobolNameResolver mCobolNameResolver;
    
	/**
	 *  The ant execute method. Generates a new Cobol data description source.
	 */
    public final void execute() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Cobol source generation started");
		}
		checkInput();
    	String outPath = mTargetDir.getPath() + File.separator
        	+ mTargetCobolFileName;
    	try {
    		String code = generate(
    				mJaxbPackageName,
    				mJaxbTypeName,
    				mCobolRootDataItemName,
    				mFirstCobolLevel,
    				mCobolLevelIncrement);
    		BufferedWriter writer = new BufferedWriter(
					new FileWriter(new File(outPath)));
    		writer.write(code);
			writer.close();
    	} catch (IOException e) {
			throw new BuildException(e);
		} catch (CobolGenerationException e) {
			throw new BuildException(e);
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Cobol source generation ended");
		}
    }

    /**
     * Checks that properties set are valid.
     */
    private void checkInput() {
    	
		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput started");
			LOG.debug("   Source JAXB package    = " + mJaxbPackageName);
			LOG.debug("   Source JAXB type name  = " + mJaxbTypeName);
			LOG.debug("   Root data item name    = " + mCobolRootDataItemName);
			LOG.debug("   First data item level  = " + mFirstCobolLevel);
			LOG.debug("   Level increment        = " + mCobolLevelIncrement);
			LOG.debug("   Target directory       = " + mTargetDir);
			LOG.debug("   Target Cobol file name = " + mTargetCobolFileName);
		}
    	/* Check that we have a valid JAXB type name.  */
    	if (mJaxbTypeName == null || mJaxbTypeName.length() == 0) {
			throw (new BuildException(
					"You must provide a JAXB type name"));
    	}
    	
    	/* Check that we have a valid target directory.  */
    	if (mTargetDir == null) {
			throw (new BuildException(
					"You must provide a target directory"));
    	}
    	if (!mTargetDir.exists()) {
			throw (new BuildException(
					"Directory " + mTargetDir + " does not exist"));
    	}
    	if (!mTargetDir.isDirectory() || !mTargetDir.canWrite()) {
			throw (new BuildException(
					mTargetDir + " is not a directory or is not writable"));
    	}
    	
    	/* Check levels */
    	if (mFirstCobolLevel < 0 || mFirstCobolLevel > 49) {
			throw (new BuildException(
					mFirstCobolLevel
					+ " is not a valid COBOL level number"));
    	}
    	if (mCobolLevelIncrement < 0 || mCobolLevelIncrement > 49) {
			throw (new BuildException(
					mCobolLevelIncrement
					+ " is not a valid COBOL level increment"));
    	}
    	
    	/* Set valid default level related values */
    	if (mFirstCobolLevel == 0) {
    		mFirstCobolLevel = 1;
    	}
    	if (mCobolLevelIncrement == 0) {
    		mCobolLevelIncrement = 1;
    	}
    	
    	/* Set a valid root cobol data item */
    	if (mCobolRootDataItemName == null
    			|| mCobolRootDataItemName.length() == 0) {
    		mCobolRootDataItemName = mJaxbTypeName;
    	}

    	/* Set a valid target generated Cobol file name */
    	if (mTargetCobolFileName == null
    			|| mTargetCobolFileName.length() == 0) {
    		mTargetCobolFileName = mJaxbTypeName + ".cbl";
    	}
 
    	if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput ended");
		}
    }
    
	/**
	 * Using the <code>cobcgen</code> utility, this will use reflection
	 * to instantiate a Jaxb object corresponding to a structure
	 * received and then generate COBOL data description code using
	 * the COBOL annotations in the jaxb class. 
	 * @param jaxbPackageName the JAXB classes package name
	 * @param jaxbType the JAXB class
	 * @param cobolRootDataItemName the COBOL structure root data item name
	 * @param firstCobolLevel where to start numbering Cobol data items
	 * @param cobolLevelIncrement how much to increment Cobol data items moving
	 *  from parent to child
	 * @return data description COBOL source code for the structure
	 * @throws CobolGenerationException if code generation fails
	 */
	public static String generate(
			final String jaxbPackageName,
			final String jaxbType,
			final String cobolRootDataItemName,
			final int firstCobolLevel,
			final int cobolLevelIncrement)
	throws CobolGenerationException {
		try {
			JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
					jaxbPackageName, jaxbType);
			Object objectFactory = elementDescriptor.createObjectFactory();
			Class < ? > clazz = elementDescriptor.loadJaxbClass();
			CComplexReflectBinding ccem = new CComplexReflectBinding(
					objectFactory, clazz);
			String cobolRootName = cobolRootDataItemName;
			if (cobolRootName == null || cobolRootName.length() == 0) {
				cobolRootName = getCobolNameResolver().getName(
						jaxbType);
			}
			ccem.setCobolName(cobolRootName);
			StringWriter writer = new StringWriter();
			BufferedWriter bufWriter = new BufferedWriter(writer);
			CobolGenVisitor cev = new CobolGenVisitor(
					firstCobolLevel, cobolLevelIncrement, bufWriter);
			ccem.accept(cev);
			bufWriter.flush();
			return writer.toString();
		} catch (ReflectBindingException e) {
			throw new CobolGenerationException(e);
		} catch (HostException e) {
			throw new CobolGenerationException(e);
		} catch (IOException e) {
			throw new CobolGenerationException(e);
		} catch (ClassNotFoundException e) {
			throw new CobolGenerationException(e);
		} catch (CobolNameResolverException e) {
			throw new CobolGenerationException(e);
		} catch (JAXBAnnotationException e) {
			throw new CobolGenerationException(e);
		}
	}
	
	/**
	 * @return the current name resolver or a new one if none existed before
	 * @throws CobolNameResolverException if resolver cannot be created
	 */
	private static CobolNameResolver getCobolNameResolver()
	throws CobolNameResolverException {
		if (mCobolNameResolver == null) {
			mCobolNameResolver = new CobolNameResolver();
		}
		return mCobolNameResolver;
	}
	
	/**
	 * @return the package name used for JAXB classes
	 */
	public final String getJaxbPackageName() {
		return mJaxbPackageName;
	}

	/**
	 * @param jaxbPackageName the JAXB classes package name to set
	 */
	public final void setJaxbPackageName(final String jaxbPackageName) {
		mJaxbPackageName = jaxbPackageName;
	}

	/**
	 * @return Returns the JAXB type name.
	 */
	public final String getJaxbTypeName() {
		return mJaxbTypeName;
	}

	/**
	 * @param objectName The JAXB type name to set.
	 */
	public final void setJaxbTypeName(
			final String objectName) {
		mJaxbTypeName = objectName;
	}

    /**
	 * @return the current target directory
	 */
	public final File getTargetDir() {
		return mTargetDir;
	}

	/**
	 * @param targetDir the target directory to set
	 */
	public final void setTargetDir(final File targetDir) {
		mTargetDir = targetDir;
	}

	/**
	 * @return the target generated Cobol file name
	 */
	public final String getTargetCobolFileName() {
		return mTargetCobolFileName;
	}

	/**
	 * @param targetCobolFileName the target generated Cobol file name to set
	 */
	public final void setTargetCobolFileName(final String targetCobolFileName) {
		mTargetCobolFileName = targetCobolFileName;
	}

	/**
	 * @return the Cobol Root Data Item Name
	 */
	public final String getCobolRootDataItemName() {
		return mCobolRootDataItemName;
	}

	/**
	 * @param cobolRootDataItemName the Cobol Root Data Item Name to set
	 */
	public final void setCobolRootDataItemName(
			final String cobolRootDataItemName) {
		mCobolRootDataItemName = cobolRootDataItemName;
	}

	/**
	 * @return where to start numbering Cobol data items
	 */
	public final int getFirstCobolLevel() {
		return mFirstCobolLevel;
	}

	/**
	 * @param firstCobolLevel where to start numbering Cobol data items
	 */
	public final void setFirstCobolLevel(final int firstCobolLevel) {
		mFirstCobolLevel = firstCobolLevel;
	}

	/**
	 * @return how much to increment Cobol data items moving from parent to
	 *  child
	 */
	public final int getCobolLevelIncrement() {
		return mCobolLevelIncrement;
	}

	/**
	 * @param cobolLevelIncrement how much to increment Cobol data items
	 *  moving from parent to child
	 */
	public final void setCobolLevelIncrement(final int cobolLevelIncrement) {
		mCobolLevelIncrement = cobolLevelIncrement;
	}

}
