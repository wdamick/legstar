/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
 * This is a generic extension that allows descendant plugins to declare new
 * library containers.
 */
public abstract class AbstractClasspathInitializer
extends ClasspathContainerInitializer {


    /** Name of the container library. */
    private String mLibraryName;

    /** Description of the container library. */
    private String mLibraryDescription;

    /** The plugin that contains a lib folder with jars to populate the
     *  container library. */
    private String mPluginId;


    /**
     * Creates the initializer with the parameter set needed to create a
     * library container.
     * @param pluginId The plugin that contains a lib folder with jars to
     *  populate the container library
     * @param libraryName Name of the container library
     * @param libraryDescription Description of the container library
     */
    public AbstractClasspathInitializer(
            final String pluginId,
            final String libraryName,
            final String libraryDescription) {
        mPluginId = pluginId;
        mLibraryName = libraryName;
        mLibraryDescription = libraryDescription;
    }

    /** {@inheritDoc} */
    public void initialize(
            final IPath containerPath,
            final IJavaProject project) throws CoreException {

        JavaCore.setClasspathContainer(
                new Path(mLibraryName), 
                new IJavaProject[]{ project },
                new IClasspathContainer[] {
                    new IClasspathContainer() {
                        public IClasspathEntry[] getClasspathEntries() {
                            return getLegStarClassPath(); 
                        }
                        public String getDescription() {
                            return mLibraryDescription;
                        }
                        public int getKind() {
                            return IClasspathContainer.K_SYSTEM;
                        }
                        public IPath getPath() {
                            return new Path(mLibraryName);
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
                    Activator.getPluginInstallLocation(mPluginId)
                    + "/lib");

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
            Activator.logCoreException(e.getTargetException(), mPluginId);
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

    /** {@inheritDoc}*/
    public boolean canUpdateClasspathContainer(final IPath containerPath,
            final IJavaProject project) {
        return true;
    }

    /** {@inheritDoc}*/
    public void requestClasspathContainerUpdate(final IPath containerPath,
            final IJavaProject project,
            final IClasspathContainer containerSuggestion)
    throws CoreException {
        JavaCore.setClasspathContainer(containerPath,
                new IJavaProject[] { project },
                new IClasspathContainer[] { containerSuggestion }, null);
    }
}
