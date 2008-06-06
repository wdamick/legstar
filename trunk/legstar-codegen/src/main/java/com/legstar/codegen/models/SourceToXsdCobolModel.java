package com.legstar.codegen.models;

import java.io.File;

/**
 * A model usable for XML Schema generation.
 * Groups all the data needed to generate an annotated XML schema from
 * a source. This is primarily intended to support velocity
 * template generation of ad-hoc ant scripts.
 * Also provides a convenience method to help with velocity generation.
 *
 */
public abstract class SourceToXsdCobolModel extends AbstractAntBuildModel {
	
	/** Package name of target JAXB classes as it appears in the generated
	 *  XSD annotations. */
	private String mJaxbPackageName;
	
	/** The target schema namespace. */
	private String mNamespace = "";
	
	/** The target directory where annotated XSD will be created. */
	private File mTargetDir;

	/** The target annotated XSD file name. */
	private String mTargetXsdFileName;
	
	/**
	 * @return the Package name of target JAXB classes
	 */
	public final String getJaxbPackageName() {
		return mJaxbPackageName;
	}

	/**
	 * @param jaxbPackageName the Package name of target JAXB classes to set
	 */
	public final void setJaxbPackageName(final String jaxbPackageName) {
		mJaxbPackageName = jaxbPackageName;
	}

	/**
	 * @return the The target schema namespace
	 */
	public final String getNamespace() {
		return mNamespace;
	}

	/**
	 * @param namespace the The target schema namespace to set
	 */
	public final void setNamespace(final String namespace) {
		/* Keep namespacelowercase so we can derive package names
		 * from it if necessary. */
		mNamespace = namespace.toLowerCase();
	}

	/**
	 * @return the The target directory
	 */
	public final File getTargetDir() {
		return mTargetDir;
	}

	/**
	 * @param targetDir the The target directory to set
	 */
	public final void setTargetDir(final File targetDir) {
		mTargetDir = targetDir;
	}

	/**
	 * @return the The target annotated XSD file name
	 */
	public final String getTargetXsdFileName() {
		return mTargetXsdFileName;
	}

	/**
	 * @param targetXsdFileName the The target annotated XSD file name to set
	 */
	public final void setTargetXsdFileName(final String targetXsdFileName) {
		mTargetXsdFileName = targetXsdFileName;
	}

}
