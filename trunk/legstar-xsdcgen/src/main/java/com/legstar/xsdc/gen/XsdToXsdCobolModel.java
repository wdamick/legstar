package com.legstar.xsdc.gen;

import java.io.File;
import java.net.URI;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * A model for XML Schema and WSDL to COBOL-annotated XML schema.
 */
public class XsdToXsdCobolModel extends SourceToXsdCobolModel {
	
	/** A URI where the XSD is available. */
	private URI mInputXsdUri;
	
	/** Suffix to be added to JAXB classes names for XML schema types. This 
	 * is used to disambiguate java object names when elements and types
	 * have the same names.*/
	private String mJaxbTypeClassesSuffix;
	
	/** A default value for the JAXB type suffix. */
	public static final String DEFAULT_JAXB_TYPE_CLASSES_SUFFIX = "Type"; 
	
	/** This generator name. */
	public static final String X2S_GENERATOR_NAME =
		"LegStar Xsd or Wsdl to Xsd generator";

	/** This velocity template. */
	public static final String X2S_VELOCITY_MACRO_NAME =
		"vlc/build-x2sc-xml.vm";
	
	/**
	 * Creates an ant build script file ready for XSD generation.
	 * @param targetFile the script file that must be created
	 * @throws CodeGenMakeException if generation fails
	 */
	public void generateBuild(
			final File targetFile) throws CodeGenMakeException {
		super.generateBuild(
				X2S_GENERATOR_NAME, X2S_VELOCITY_MACRO_NAME, targetFile);
	}

	/**
	 * @return the input XML schema uri
	 */
	public final URI getInputXsdUri() {
		return mInputXsdUri;
	}

	/**
	 * @param xsdUri the input XML schema uri to set
	 */
	public final void setInputXsdUri(
			final URI xsdUri) {
		mInputXsdUri = xsdUri;
	}

	/**
	 * @return the Suffix to be added to JAXB classes names for XML schema types
	 */
	public final String getJaxbTypeClassesSuffix() {
		if (mJaxbTypeClassesSuffix == null) {
			return DEFAULT_JAXB_TYPE_CLASSES_SUFFIX;
		}
		return mJaxbTypeClassesSuffix;
	}

	/**
	 * @param jaxbTypeClassesSuffix the Suffix to be added to JAXB classes names
	 *  for XML schema types
	 */
	public final void setJaxbTypeClassesSuffix(
			final String jaxbTypeClassesSuffix) {
		mJaxbTypeClassesSuffix = jaxbTypeClassesSuffix;
	}

}
