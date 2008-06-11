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
package com.legstar.eclipse.plugin.cixsmap.wizards;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import com.legstar.eclipse.plugin.cixsmap.dialogs.LegacyStructureDialog;

/**
 * Users selects the target container in the Eclipse workspace as well as
 * a file name. The file will contain the mapping description between
 * CICS programs and Java/Web Services methods.
 */
public class NewMappingFileWizardPage extends WizardNewFileCreationPage {

	/** Page name. */
	private static final String PAGE_NAME =
		"NewWSWizardPage";
	
	/** Page title text. */
	private static final String PAGE_TITLE =
		"Create an operations mapping file";
	
	/** Page description text. */
	private static final String PAGE_DESC =
		"Map CICS programs to Java/Web Services operations";
	
	/** New mapping file name. */
	private static final String NEW_WS_NAME =
		"Mapping file name:";
	
	 /**
	 * Constructor for NewWSWizardPage.
	 * 
	 * @param selection current selection
	 */
	public NewMappingFileWizardPage(
			final IStructuredSelection selection) {
		super(PAGE_NAME, selection);
		setTitle(PAGE_TITLE);
		setDescription(PAGE_DESC);
	}
	
	/**
	 * Overriding this to prevent WizardNewFileCreationPage from displaying
	 * the advanced button which was not tested enough and is somewhat
	 * confusing for newcomers.
	 * @param parent the parent container
	 *  */
	protected void createAdvancedControls(final Composite parent) {
        Preferences preferences =
        	ResourcesPlugin.getPlugin().getPluginPreferences();
        preferences.setValue(ResourcesPlugin.PREF_DISABLE_LINKING, true);
        setFileExtension("cixs");
        super.createAdvancedControls(parent);
	}
	
    /** {@inheritDoc} */
    protected boolean validatePage() {
        if (!super.validatePage()) {
            return false;
        }
        
        /* Only Java projects are valid containers. */
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        IResource resource = root.findMember(getContainerFullPath());
        IJavaProject jproject = JavaCore.create(resource.getProject());
        try {
            /* Check that there are LegStar binding classes in this project */
            IPackageFragmentRoot[] pkgRoots =
                jproject.getPackageFragmentRoots();
            for (int j = 0; j < pkgRoots.length; j++) {
                IPackageFragmentRoot pkgRoot = pkgRoots[j];
                if (pkgRoot.getKind() ==  IPackageFragmentRoot.K_SOURCE) {
                    for (int k = 0; k < pkgRoot.getChildren().length; k++) {
                        IJavaElement el = pkgRoot.getChildren()[k];
                        if (el.getPath().lastSegment().compareTo(
                        		LegacyStructureDialog.BIND_FRAG) == 0) {
                            return true;
                        }
                    }
                }
            }
            setErrorMessage("Project " + resource.getProject().getName()
                    + " does not contain LegStar binding classes");
            return false;
        } catch (JavaModelException e) {
            setErrorMessage("Project " + resource.getProject().getName()
                    + " is not a Java project");
            return false;
        }
    }

    /**
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#getNewFileLabel()
     * @return the label to display in the file name specification visual
     *     component group
     * {@inheritDoc}
	 */
	protected final String getNewFileLabel() {
		return NEW_WS_NAME;
	}

	/**
	 * @return the container name
	 */
	public final String getContainerName() {
		return getContainerFullPath().toOSString();
	}

	/**
	 * @return the mapping file name
	 */
	public final String getMappingFileName() {
		return getFileName();
	}

}
