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
package com.legstar.eclipse.plugin.cixsmap.wizards;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.eclipse.plugin.cixsmap.Messages;
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
	
	
	 /**
	 * Constructor for NewWSWizardPage.
	 * 
	 * @param selection current selection
	 */
	public NewMappingFileWizardPage(
			final IStructuredSelection selection) {
		super(PAGE_NAME, selection);
		setTitle(Messages.new_file_wizard_page_title);
		setDescription(Messages.new_file_wizard_description);
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(
                		com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID,
                		com.legstar.eclipse.plugin.common.Activator.LOGO_IMG);
        setImageDescriptor(image);
	}
	
	/** {@inheritDoc} */
	public void createControl(final Composite parent) {
		super.createControl(parent);
		setFileName('.' + Messages.operations_mapping_file_suffix);
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
        super.createAdvancedControls(parent);
	}
	
    /** {@inheritDoc} */
    protected boolean validatePage() {
        setCanFinish(false);
        if (!super.validatePage()) {
            return false;
        }
        
        /* There must be a mapping file name */
        if (getFileName() == null || getFileName().length() == 0) {
			setErrorMessage(NLS.bind(
					Messages.invalid_mapping_file_msg,
					getFileName()));
            return false;
        }
        
        /* There must be more than the mere mapping file name extension */
    	IPath path = new Path(getFileName());
    	IPath noExtensionPath = path.removeFileExtension();
    	if (noExtensionPath.isEmpty()) {
			setErrorMessage(NLS.bind(
					Messages.invalid_mapping_file_msg,
					getFileName()));
            return false;
    	}
        
        /* Make sure the file name has the correct extension */
    	String extension = path.getFileExtension();
		if (extension == null || !extension.equals(
				Messages.operations_mapping_file_suffix)) {
			setErrorMessage(NLS.bind(
					Messages.invalid_mapping_file_extension_msg,
					extension, Messages.operations_mapping_file_suffix));
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
                            setCanFinish(true);
                            return true;
                        }
                    }
                }
            }
            setErrorMessage(NLS.bind(Messages.no_coxb_classes_in_project_msg,
            				resource.getProject().getName()));
            return false;
        } catch (JavaModelException e) {
            setErrorMessage(NLS.bind(Messages.invalid_java_project_msg,
            		resource.getProject().getName(), e.getMessage()));
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
		return Messages.new_operations_mapping_file_name_label;
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
	
	/**
	 * Tells the wizard wether its ok to finish.
	 * @param canFinish true or false
	 */
	private void setCanFinish(final boolean canFinish) {
		((NewMappingFileWizard) getWizard()).setCanFinish(canFinish);
	}
	
}
