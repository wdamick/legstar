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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.util.JaxbUtil;
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
			BufferedWriter writer = new BufferedWriter(
					new FileWriter(new File(outPath)));
			CobolGenVisitor cev = new CobolGenVisitor(writer);
			CComplexReflectBinding ccem = getBinding();
			ccem.setCobolName(
					cev.getNameResolver().getName(mCobolRootDataItemName));
			ccem.accept(cev);
			writer.close();
    	} catch (IOException e) {
			throw new BuildException(e);
		} catch (HostException e) {
			throw new BuildException(e);
		} catch (CobolNameResolverException e) {
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
     * Creates an instance of a JAXB object factory and creates a binding
     * for a JAXB class of the given type.
     * @return a Cobol binding
     */
    private CComplexReflectBinding getBinding() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Binding JAXB type started");
		}
    	String ofClassName = "ObjectFactory";
    	String oClassName = mJaxbTypeName;
    	if (mJaxbPackageName != null && mJaxbPackageName.length() > 0) {
    		ofClassName = mJaxbPackageName + '.' + ofClassName;
    		oClassName = mJaxbPackageName + '.' + oClassName;
    	}
    	try {
			Class < ? > ofClass = JaxbUtil.loadClass(ofClassName);
			Object of = ofClass.newInstance();
			Class < ? > oClass = JaxbUtil.loadClass(oClassName);
			if (LOG.isDebugEnabled()) {
				LOG.debug("Binding JAXB type ended");
			}
			return new CComplexReflectBinding(of, oClass);
		} catch (ClassNotFoundException e) {
			throw new BuildException(e);
		} catch (InstantiationException e) {
			throw new BuildException(e);
		} catch (IllegalAccessException e) {
			throw new BuildException(e);
		} catch (ReflectBindingException e) {
			throw new BuildException(e);
		}
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

}
