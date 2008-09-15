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
package com.legstar.schemagen;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Path;

import com.legstar.codegen.tasks.SourceToXsdCobolTask;

import java.io.File;
import java.util.Locale;
import java.util.Vector;

/**
 * This class implements an ant task to generate annotated XML schema from
 * a cobol file data descriptions.
 * 
 * @author Fady Moussallam
 */
public class COXBSchemaGenerator extends SourceToXsdCobolTask  {


	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(COXBSchemaGenerator.class);
	/** Extension used when XML schema name is built. */

	private static final String XSD_EXT = ".xsd";

	/** True will produce extra traces. */
	private boolean mDebugMode = false;

	/** Creates a root element so that all simple elements at the 01
	 * level are included in it. */
	private String mRootName = null;

	/** Fully qualified file name of cobol file. */
	private String mSourceCobolFilePath = null;

	/** Alternative way to specify cobol files using a path collection. */
	private Vector < Path > mCobolPaths = new Vector < Path >();

	/** An instance of the JNI wrapper for Schema generation. */
	private COB2XSDJNIWrapper mSchemagen;

	/** No arg constructor. */
	public COXBSchemaGenerator() {
		mSchemagen = new COB2XSDJNIWrapper();
		setModel(new CobolToXsdCobolModel());
	}

	/**
	 *  The ant method. Generates an XML schema by invoking thru JNI the c
	 *  Cobol parser and XML schema generator.
	 */
	public final void execute() {

		/* Control inputs */
		checkInput();
		if (mCobolPaths.isEmpty()) {
			generateASchema(mSourceCobolFilePath,
					getTargetDir().getAbsolutePath() + File.separator
					+ getTargetXsdFileName(),
					getNamespace(), getJaxbPackageName());
		} else {
			generateSchemas(getTargetDir().getAbsolutePath());
		}

	}

	/**
	 * Generates a single XML schema from a COBOL fragment.
	 * @param cobolFile source file for COBOL fragment
	 * @param xsdFile resulting XML schema file
	 * @param namespace namespace to use as target for XSD
	 * @param packageName the target package for generated JAXB classes
	 */
	private void generateASchema(
			final String cobolFile,
			final String xsdFile,
			final String namespace,
			final String packageName) {

		COB2XSDJNIWrapper.InVars inVars = mSchemagen.new InVars();
		COB2XSDJNIWrapper.OutVars outVars = mSchemagen.new OutVars();

		inVars.debugMode = mDebugMode;
		inVars.inRootName = mRootName;
		inVars.inFile = cobolFile;
		inVars.outFile = xsdFile;

		/* Not all possible options are exposed for now */
		inVars.xsdOptions = mSchemagen.new XsdOptions();
		inVars.cobolOptions = mSchemagen.new CobolOptions();

		inVars.xsdOptions.xsnsNs = namespace;
		inVars.xsdOptions.xsjaxbPackage = packageName;


		int resp = mSchemagen.cob2xsd(inVars, outVars);
		if (resp != 0) {
			throw (new BuildException(outVars.message));

		}
	}

	/**
	 * A collection of cobol files from the path are processed. Each
	 * generating a different XML schema.
	 * @param xsdDir the target folder for XML schemas (should end with slash)
	 */
	private void generateSchemas(final String xsdDir) {
		for (Path path : mCobolPaths) {
			String[] includedFiles = path.list(); 
			for (int i = 0; i < includedFiles.length; i++) {
				String pathname = includedFiles[i].replace('\\', '/');
				String filename = pathname.substring(
						pathname.lastIndexOf("/") + 1);
				String xsdName;
				if (filename.lastIndexOf(".") > -1) {
					xsdName = xsdDir + File.separator + filename.substring(0,
							filename.lastIndexOf(".")).toLowerCase(
									Locale.getDefault()) + XSD_EXT;
				} else {
					xsdName = xsdDir + File.separator + filename.toLowerCase()
					+ XSD_EXT;
				}
				String namespace;
				if (getNamespace().charAt(getNamespace().length() - 1) != '/') {
					namespace = getNamespace() + '/'
					+ filename.toLowerCase(Locale.getDefault());
				} else {
					namespace = getNamespace()
					+ filename.toLowerCase(Locale.getDefault());
				}
				String packageName = getJaxbPackageName()
				+ '.' + filename.toLowerCase(Locale.getDefault());
				generateASchema(pathname, xsdName, namespace, packageName);
			}
		}
	}

	/**
	 * Check the mandatory options.
	 */
	private void checkInput() {
		
		super.checkInput(false, true);

		if (LOG.isDebugEnabled()) {
			LOG.debug("   Cobol file path     = " + mSourceCobolFilePath);
			if (!mCobolPaths.isEmpty()) {
				for (Path path : mCobolPaths) {
					LOG.debug("   Cobol path           = "
							+ path);
				}
			}
		}

		if (mSourceCobolFilePath == null
				|| mSourceCobolFilePath.length() == 0) {
			if (mCobolPaths.isEmpty()) {
				throw (new BuildException(
				"You must specify a cobol file name or a path"));
			}
		} else {
			if (!mCobolPaths.isEmpty()) {
				throw (new BuildException(
						"You must specify either a cobol file name"
						+ " or a path (but not both)"));
			}
		}

		if (mCobolPaths.isEmpty()) {
			if (getTargetXsdFileName() == null 
					|| getTargetXsdFileName().length() == 0) {
				throw (new BuildException(
					"You must specify an output XML schema file name"));
			}
		} else {
			if (getTargetXsdFileName() != null 
					&& getTargetXsdFileName().length() > 0) {
				throw (new BuildException(
						"You should not specify an XML schema file name when a"
						+ " path is provided"));
			}
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput ended");
		}
	}

	/**
	 * @return the debug mode
	 */
	public final boolean isDebugMode() {
		return mDebugMode;
	}

	/**
	 * @param debugMode the debug mode to set
	 */
	public final void setDebugMode(final boolean debugMode) {
		mDebugMode = debugMode;
	}

	/**
	 * @return the root name element to create if any
	 */
	public final String getRootName() {
		return mRootName;
	}

	/**
	 * @param rootName the root name element to set
	 */
	public final void setRootName(final String rootName) {
		mRootName = rootName;
	}

	/**
	 * @deprecated
	 * Use <code>getSourceCobolFilePath</code> instead
	 * @return the fully qualified name of the Cobol file to parse
	 */
	public final String getCobolFile() {
		return mSourceCobolFilePath;
	}

	/**
	 * @deprecated
	 * Use <code>setSourceCobolFilePath</code> instead
	 * @param cobolFile the Cobol file name to set
	 */
	public final void setCobolFile(final String cobolFile) {
		mSourceCobolFilePath = cobolFile;
	}

	/**
	 * @return the full path of the Cobol file to parse
	 */
	public final String getSourceCobolFilePath() {
		return mSourceCobolFilePath;
	}

	/**
	 * @param sourceCobolFilePath the full path of the Cobol file to parse
	 */
	public final void setSourceCobolFilePath(final String sourceCobolFilePath) {
		mSourceCobolFilePath = sourceCobolFilePath;
	}

	/**
	 * @deprecated
	 * Use <code>getTargetXsdFileName</code> instead
	 * @return the fully qualified XML schema file to produce
	 */
	public final String getXSDFile() {
		return getTargetDir() + File.separator + getTargetXsdFileName();
	}

	/**
	 * @deprecated
	 * Use <code>setTargetXsdFileName</code> instead
	 * @param file the XML schema file to set
	 */
	public final void setXSDFile(final String file) {
		String normalizedFileName = file.replace(File.separator, "/");
		int i =  normalizedFileName.lastIndexOf("/");
		if (i > 0) {
			String lastSegment = normalizedFileName.substring(i + 1);
			if (lastSegment.contains(".")) {
				setTargetDir(new File(normalizedFileName.substring(0, i)));
				setTargetXsdFileName(normalizedFileName.substring(i + 1));
			} else {
				setTargetDir(new File(normalizedFileName));
			}
		} else {
			setTargetXsdFileName(normalizedFileName);
		}
	}

	/**
	 * @deprecated
	 * Use <code>getJaxbPackageName</code> instead
	 * @return the generated Java classes package
	 */
	public final String getPackage() {
		return getJaxbPackageName();
	}

	/**
	 * @deprecated
	 * Use <code>setJaxbPackageName</code> instead
	 * @param package1 the generated Java classes package to set
	 */
	public final void setPackage(final String package1) {
		setJaxbPackageName(package1);
	}

	/**
	 * Add a path to the path list.
	 * @param path a path element containing cobol files
	 */
	public final void addPath(final Path path) {
		mCobolPaths.add(path);
	}

}
