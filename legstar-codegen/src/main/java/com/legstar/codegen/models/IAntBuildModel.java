package com.legstar.codegen.models;

import java.io.File;

import com.legstar.codegen.CodeGenMakeException;

/**
 * Generators that create ant builds will implement this interface.
 *
 */
public interface IAntBuildModel {
	
	/**
	 * Create an ant build script ready to run.
	 * @param scriptFile the build file
	 * @throws CodeGenMakeException if build cannot be generated
	 */
	void generateBuild(File scriptFile) throws CodeGenMakeException;

	/**
	 * @return the temporary file used to check whether the ant script succeeded
	 */
	File getProbeFile();

	/**
	 * @param probeFile the temporary file used to check whether the ant
	 *  script succeeded to set
	 */
	void setProbeFile(final File probeFile);
}
