package com.legstar.coxb.gen;

import java.io.File;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Gathers all parameters that are needed during the lifetime of the generation
 * process. This allows more parameters to be added without too much impact on
 * other classes.
 *
 */
public class CoxbGenContext {

	/** The JAXB/COXB annotated XML schema file. */
	private File mXsdFile;
	
	/** The package name used for JAXB classes. */
	private String mJaxbPackageName;
	
	/** The target package name for generated binding classes. */
	private String mCoxbPackageName;
	
	/** The location where JAXB classes live. */
	private File mJaxbDir;
	
	/** The JAXB root object name. */
	private String mJaxbRootObjectName;
	
	/** The target directory where source files will be created. */
	private File mTargetDir;
	
	/** An optional runtime alternative to the Jaxb package name used at
	 * generation time. */
	private String mAlternativePackageName;

	/** At runtime, if a alternativePackageName is specified, this alternative
	 * factory can be used rather than the JAXB one. */
	private String mAlternativeFactoryName;
	
	/** When a alternativePackageName is specified, each JAXB class name can
	 * be mapped to an alternative class name. */
	private List < AlternativeClassName > mAlternativeClassNameMap;
	
	/** The additional package level for generated binding classes. */
	private static final String COXB_PACKAGE_SUFFIX = "bind";
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CoxbGenContext.class);
	
	/**
	 * Provides a complete trace of parameters values.
	 */
	public final void traceContext() {
		LOG.debug("   JAXB classes location           ="
				+ " " + getJaxbDir());
		LOG.debug("   JAXB Package name               ="
				+ " " + getJaxbPackageName());
		LOG.debug("   JAXB root object name           ="
				+ " " + getJaxbRootObjectName());
		LOG.debug("   COBOL annotated XML schema file ="
				+ " " + getXsdFile());
		LOG.debug("   Binding classes target location ="
				+ " " + getTargetDir());
		LOG.debug("   Binding classes Package name    ="
				+ " " + getCoxbPackageName());
		LOG.debug("   Alternative package name        ="
				+ " " + getAlternativePackageName());
		LOG.debug("   Alternative factory name        ="
				+ " " + getAlternativeFactoryName());
		if (mAlternativeClassNameMap != null) {
			for (AlternativeClassName rename : mAlternativeClassNameMap) {
				LOG.debug("   Rename Jaxb class "
						+ rename.getJaxbClassName() + " to " 
						+ rename.getToClassName());
			}
		}
	}

	/**
	 * @return Returns the JAXB root object name.
	 */
	public final String getJaxbRootObjectName() {
		return mJaxbRootObjectName;
	}

	/**
	 * @param objectName The JAXB root object name to set.
	 */
	public final void setJaxbRootObjectName(
			final String objectName) {
		mJaxbRootObjectName = objectName;
	}

	/**
	 * @return the XML schema file
	 */
	public final File getXsdFile() {
		return mXsdFile;
	}

	/**
	 * @param xsdFile the XML schema file to set
	 */
	public final void setXsdFile(
			final File xsdFile) {
		mXsdFile = xsdFile;
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
	 * @return the location where JAXB classes live
	 */
	public final File getJaxbDir() {
		return mJaxbDir;
	}

	/**
	 * @param jaxbDir the JAXB location to set
	 */
	public final void setJaxbDir(final File jaxbDir) {
		mJaxbDir = jaxbDir;
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
	 * @return the package name for generated binding classes
	 */
	public final String getCoxbPackageName() {
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
	public final void setCoxbPackageName(final String coxbPackageName) {
		mCoxbPackageName = coxbPackageName;
	}

	/**
	 * @return the optional runtime alternative to the Jaxb package name used at
	 * generation time
	 */
	public final String getAlternativePackageName() {
		return mAlternativePackageName;
	}

	/**
	 * @param alternativePackageName the optional runtime alternative to the
	 * Jaxb package name used at generation time
	 */
	public final void setAlternativePackageName(
			final String alternativePackageName) {
		mAlternativePackageName = alternativePackageName;
	}

	/**
	 * @return the alternate factory to used rather than the JAXB one.
	 */
	public final String getAlternativeFactoryName() {
		return mAlternativeFactoryName;
	}

	/**
	 * @param targetFactoryName the alternate factory to used rather than the
	 * JAXB one
	 */
	public final void setAlternativeFactoryName(
			final String targetFactoryName) {
		mAlternativeFactoryName = targetFactoryName;
	}
	
	/**
	 * @return a list of alternative class names to Jaxb class names
	 */
	public final List < AlternativeClassName > getAlternativeClassNameMap() {
		return mAlternativeClassNameMap;
	}
	
	/**
	 * Translate a jaxb class name into an alternative class name if one is
	 * found into the AlternativeClassNameMap.
	 * @param jaxbClassName the jaxb class name to be renamed
	 * @return a new name if one is found, the original name otherwise
	 */
	public final String getAlternativeClassName(final String jaxbClassName) {
		if (mAlternativeClassNameMap != null) {
			for (AlternativeClassName aClassName : mAlternativeClassNameMap) {
				if (aClassName.getJaxbClassName().equals(jaxbClassName)) {
					return aClassName.getToClassName();
				}
			}
		}
		return jaxbClassName;
	}
	
	/**
	 * @param alternativeClassNameMap a list of alternative class names to Jaxb
	 * class names
	 */
	public final void setAlternativeClassNameMap(
			final List < AlternativeClassName > alternativeClassNameMap) {
		mAlternativeClassNameMap = alternativeClassNameMap;
	}


}
