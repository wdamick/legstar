package com.legstar.j2sc.gen;

import java.io.File;
import java.util.List;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * A model for Java to COBOL-annotated XML schema.
 */
public class JavaToXsdCobolModel extends SourceToXsdCobolModel {
	
	/** List of fully qualified java class names. */
	private List < String > mClassNames;
	
	/** List of path elements locations to be used as the classpath in
	 * order to locate the java classes. */
	private List < String > mPathElementLocations;
	
	/** This generator name. */
	public static final String J2S_GENERATOR_NAME =
		"LegStar Java to Xsd generator";

	/** This velocity template. */
	public static final String J2S_VELOCITY_MACRO_NAME =
		"vlc/build-j2sc-xml.vm";
	
	/**
	 * Creates an ant build script file ready for XSD generation.
	 * @param targetFile the script file that must be created
	 * @throws CodeGenMakeException if generation fails
	 */
	public void generateBuild(
			final File targetFile) throws CodeGenMakeException {
		super.generateBuild(
				J2S_GENERATOR_NAME, J2S_VELOCITY_MACRO_NAME, targetFile);
	}

	/**
	 * @return the List of fully qualified java class names
	 */
	public final List < String > getClassNames() {
		return mClassNames;
	}

	/**
	 * @param classNames the List of fully qualified java class names to set
	 */
	public final void setClassNames(final List < String > classNames) {
		mClassNames = classNames;
	}

	/**
	 * @return the List of path elements locations
	 */
	public final List < String > getPathElementLocations() {
		return mPathElementLocations;
	}

	/**
	 * @param pathElementLocations the List of path elements locations to set
	 */
	public final void setPathElementLocations(
			final List < String > pathElementLocations) {
		mPathElementLocations = pathElementLocations;
	}

}
