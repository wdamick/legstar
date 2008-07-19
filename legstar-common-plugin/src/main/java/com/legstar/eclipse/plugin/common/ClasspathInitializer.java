/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.common;

import java.io.File;
import java.io.FilenameFilter;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

/**
 * This extension allows us to declare the LegStar library container which
 * projects might use to get all necessary LegStar dependencies.
 *
 */
public class ClasspathInitializer extends ClasspathContainerInitializer {

	/** The library container identifier for LegStar. */
	public static final String LIBRARY_NAME = "com.legstar.eclipse.LIBRARY";
	
	/** The library container description. */
	public static final String LIBRARY_DESCRIPTION =
		"LegStar library container";
	
	/** {@inheritDoc} */
	public void initialize(
			final IPath containerPath,
			final IJavaProject project) throws CoreException {

		JavaCore.setClasspathContainer(
				new Path(LIBRARY_NAME), 
				new IJavaProject[]{ project },
				new IClasspathContainer[] {
					new IClasspathContainer() {
						public IClasspathEntry[] getClasspathEntries() {
							return getLegStarClassPath(); 
						}
						public String getDescription() {
							return LIBRARY_DESCRIPTION;
						}
						public int getKind() {
							return IClasspathContainer.K_SYSTEM;
						}
						public IPath getPath() {
							return new Path(LIBRARY_NAME);
						}
					}			
				}, 
				null);
		
	}

	/**
	 * Collects all jar files from installation folder and creates a set of
	 * classpath entries.
	 * In case classpath entries cannot be  created this logs an exception
	 * but let it slip. User should still be able to setup the classpath
	 * manually.
	 * @return an array of classpath entries or null if none is built
	 */
	private IClasspathEntry[] getLegStarClassPath() {
		try {
			java.util.List < IClasspathEntry > pathEntries =
				new ArrayList < IClasspathEntry >();
			File libraryFolder = new File(
					Activator.getPluginInstallLocation() + "/lib");
			
			/* Add all jar files to classpath */
			File[] files = libraryFolder.listFiles(new JarFilter());
			for (File file : files) {
				pathEntries.add(
						JavaCore.newLibraryEntry(
								new Path(file.getAbsolutePath()),
								null, null, false));
			}
			
			return (IClasspathEntry[]) pathEntries.toArray(
					new IClasspathEntry[pathEntries.size()]);
		} catch (InvocationTargetException e) {
			Activator.logCoreException(e.getTargetException(),
					Activator.PLUGIN_ID);
			return null;
		}
	}
	
	/**
	 * Used to limit search to jar files.
	 */
	private class JarFilter implements FilenameFilter {
		/** {@inheritDoc} */
	    public boolean accept(final File dir, final String name) {
	        return (name.endsWith(".jar"));
	    }
	}

}
