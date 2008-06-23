package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
import com.legstar.eclipse.plugin.cixscom.wizards
		.AbstractCixsGeneratorWizardPage;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * A wizard page displaying widgets that are specific to the Jaxws to Cixs
 * generation.
 */
public class Jaxws2CixsGeneratorWizardPage
        extends AbstractCixsGeneratorWizardPage {

    /** Page name. */
    private static final String PAGE_NAME = "Jaxws2CixsGeneratorWizardPage";
    
    /** J2ee folder where web deployment files should be generated. */
    private Text mTargetWDDDirText = null;
    
    /** J2ee folder where war files should be deployed. */
    private Text mTargetWarDirText = null;
    
    /** Generated Web services target namespace. */
    private Text mTargetNamespaceText = null;
    
    /**
     * Construct the page.
     * @param selection the current workbench selection
     * @param mappingFile the mapping file
     */
    protected Jaxws2CixsGeneratorWizardPage(
            final IStructuredSelection selection,
            final IFile mappingFile) {
        super(selection, PAGE_NAME,
        		Messages.jaxws_to_cixs_wizard_page_title,
        		Messages.jaxws_to_cixs_wizard_page_description,
        		mappingFile);
    }

    /** {@inheritDoc} */
    public void addWidgetsToCixsGroup(final Composite container) {
    	mTargetNamespaceText = createTextField(container, getStore(),
                "targetNamespace",
                Messages.adapter_target_namespace_label + ':');
    	mTargetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
    }

    /** {@inheritDoc} */
    public void addWidgetsToTargetGroup(final Composite container) {
        mTargetWDDDirText = createDirectoryFieldEditor(container,
                "targetWDDDir",
                Messages.wdd_target_location_label + ':');
        mTargetWDDDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
    }

    /** {@inheritDoc} */
    public void addWidgetsToCoxbGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToDeploymentGroup(final Composite container) {
        mTargetWarDirText = createTextField(container, getStore(),
                "targetJarDir",
                Messages.war_deployment_location_label + ':');
        mTargetWarDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
    }

    /** {@inheritDoc} */
    public void initExtendedWidgets(final IProject project) {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        IPath projectPath = project.getProject().getLocation();
        String j2eeWDDFolder = store.getString(
                PreferenceConstants.J2EE_WDD_FOLDER);
        setTargetWDDDir(projectPath.append(
                new Path(j2eeWDDFolder)).toOSString());
        setTargetWarDir(store.getDefaultString(
				PreferenceConstants.J2EE_WAR_FOLDER));
        initPackageName(getServiceName());
    }

	/**
	 * Package name is built from a prefix stored in preferences and the
	 * name of the target service.
	 * @param serviceName the service name
	 */
	private void initPackageName(final String serviceName) {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		String prefix = store.getString(
				PreferenceConstants.WS_TARGET_NAMESPACE_PREFIX);
		if (prefix == null || prefix.length() == 0) {
			setTargetNamespace(serviceName);
		} else {
			setTargetNamespace(prefix + '/' + serviceName);
		}
	}

	/** {@inheritDoc} */
    public boolean validateExtendedWidgets() {
        if (!checkDirectory(getTargetWDDDir(),
        		Messages.invalid_wdd_target_location_msg)) {
            return false;
        }
        if (getTargetNamespace().length() == 0) {
			updateStatus(Messages.invalid_target_namespace_msg);
			return false;
        }

        return true;
    }

    /**
     * @param targetWarDir J2ee folder where war files should be
     *  deployed
     */
    public void setTargetWarDir(final String targetWarDir) {
        mTargetWarDirText.setText(targetWarDir);
    }
    
    /**
     * @return J2ee folder where war files should be deployed
     */
    public String getTargetWarDir() {
        return mTargetWarDirText.getText();
    }

    /**
     * @param targetWDDDir J2ee folder where web deployment files should
     *  be generated
     */
    public void setTargetWDDDir(final String targetWDDDir) {
        mTargetWDDDirText.setText(targetWDDDir);
    }
    
    /**
     * @return J2ee folder where web deployment files should be generated
     */
    public String getTargetWDDDir() {
        return mTargetWDDDirText.getText();
    }

    /**
     * @param targetNamespace Generated Web services target namespace
     */
    public void setTargetNamespace(final String targetNamespace) {
        mTargetNamespaceText.setText(targetNamespace);
    }
    
    /**
     * @return Generated Web services target namespace
     */
    public String getTargetNamespace() {
        return mTargetNamespaceText.getText();
    }

    /** {@inheritDoc} */
    public AbstractCixsActivator getActivator() {
        return Activator.getDefault();
    }

}
