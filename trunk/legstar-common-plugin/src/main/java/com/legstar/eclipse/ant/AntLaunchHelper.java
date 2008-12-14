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
package com.legstar.eclipse.ant;

import java.util.List;

import org.eclipse.ant.internal.ui.launchConfigurations.AntLaunchShortcut;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;


/**
 * This is an implementation of an Eclipse launcher for ant. The objective is to
 * overcome the limitations of <code>org.eclipse.ant.core.AntRunner</code> which
 * is unable to handle the JNI limitations that prevents a DLL from being loaded
 * twice by 2 different class loaders. AntRunner creates a new class loader for
 * each execution, therefore the second execution always fails with a
 * UnsatisfiedLinkError. This was largely inspired by
 * http://dev.eclipse.org/newslists/news.eclipse.platform/msg35076.html
 * TODO This usage is not recommended so this must be a considered temporary
 * while waiting for Eclipse to provide a proper launch API.
 */
@SuppressWarnings("restriction")
public class AntLaunchHelper {

    /** The ant build file. */
    IFile mAntFile = null;

    /**
     * Create a launcher for a given ant sript file.
     * @param antFile the ant script file
     */
    public AntLaunchHelper(final IFile antFile) {
        mAntFile = antFile;
    }

    /**
     * Launch the background execution. Must be in new instance of a VM
     * to avoid any UnsatisfiedLinkError with JNI.
     * @param monitor the progress monitor
     * @return a launch configuration
     * @throws CoreException if launch fails
     */
    @SuppressWarnings({ "unchecked" })
    public ILaunch execute(
            final IProgressMonitor monitor) throws CoreException {

        List configs =
            AntLaunchShortcut.findExistingLaunchConfigurations(getAntFile());
        ILaunchConfigurationWorkingCopy launchCopy;
        if (configs.size() == 0) {
            ILaunchConfiguration config =
                AntLaunchShortcut.createDefaultLaunchConfiguration(
                        getAntFile());
            launchCopy = config.getWorkingCopy();
        } else {
            launchCopy =
                ((ILaunchConfiguration) configs.get(0)).getWorkingCopy();
        }
        return launchCopy.launch(ILaunchManager.RUN_MODE, monitor);
    }


    /**
     * @return the current ant script file
     */
    public IFile getAntFile() {
        return mAntFile;
    }

}
