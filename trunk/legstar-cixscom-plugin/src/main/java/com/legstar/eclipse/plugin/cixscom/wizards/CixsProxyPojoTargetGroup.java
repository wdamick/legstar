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
package com.legstar.eclipse.plugin.cixscom.wizards;

import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.ui.IJavaElementSearchConstants;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.window.Window;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.SelectionDialog;

import com.legstar.cixs.gen.model.options.PojoParameters;
import com.legstar.eclipse.plugin.cixscom.Activator;
import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.cixscom.preferences.PreferenceConstants;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;

/**
 * Holds the controls for a POJO proxy target.
 * <p/>
 * A POJO is described by a class name and a method name.
 *
 */
public class CixsProxyPojoTargetGroup extends AbstractCixsControlsGroup {

    /** Target POJO class name. */
    private Text mClassNameText = null;

    /** Target POJO method name. */
    private Text mMethodNameText = null;

    /**
     * Construct this control holder attaching it to a wizard page.
     * @param wizardPage the parent wizard page
     */
    public CixsProxyPojoTargetGroup(final AbstractCixsGeneratorWizardPage wizardPage) {
        super(wizardPage);
    }

    /**
     * {@inheritDoc} 
     */
    public void createButton(final Composite composite) {
        super.createButton(composite, "POJO");
    }

    /**
     * {@inheritDoc} 
     */
    public void createControls(final Composite composite) {
        super.createControls(composite, Messages.target_pojo_group_label, 3);

        AbstractWizardPage.createLabel(getGroup(), Messages.target_pojo_class_name_label + ':');
        mClassNameText = AbstractWizardPage.createText(getGroup());

        Button browseButton = AbstractWizardPage.createButton(getGroup(),
                com.legstar.eclipse.plugin.common.Messages.browse_button_label);
        browseButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                try {
                    SelectionDialog dialog = JavaUI.createTypeDialog(
                            getShell(),
                            PlatformUI.getWorkbench().getProgressService(),
                            SearchEngine.createWorkspaceScope(),
                            IJavaElementSearchConstants.CONSIDER_CLASSES,
                            false);
                    if (Window.OK == dialog.open()) {
                        Object[] results = dialog.getResult();
                        if (results != null && results.length > 0) {
                            setClassName(((IType) results[0])
                                    .getFullyQualifiedName());
                        }
                    }
                } catch (JavaModelException e1) {
                    AbstractWizard.errorDialog(getShell(),
                            Messages.class_selection_error_dialog_title,
                            Activator.PLUGIN_ID,
                            Messages.class_selection_error_short_msg,
                            NLS.bind(Messages.class_selection_error_long_msg,
                                    e1.getMessage()));
                    AbstractWizard.logCoreException(e1, Activator.PLUGIN_ID);
                }
            }
        });

        AbstractWizardPage.createLabel(getGroup(), Messages.target_pojo_method_name_label + ':');
        mMethodNameText = AbstractWizardPage.createText(getGroup());
    }

    /**
     * {@inheritDoc} 
     */
    public void createExtendedListeners() {

        mClassNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mMethodNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
    }

    /**
     * {@inheritDoc} 
     */
    public void initExtendedControls() {
        setClassName(getProjectPreferences().get(
                PreferenceConstants.PROXY_LAST_POJO_CLASS_NAME, ""));
        setMethodName(getProjectPreferences().get(
                PreferenceConstants.PROXY_LAST_POJO_METHOD_NAME, ""));
    }

    /**
     * {@inheritDoc} 
     */
    public void storeExtendedProjectPreferences() {
        getProjectPreferences().put(
                PreferenceConstants.PROXY_LAST_POJO_CLASS_NAME, getClassName());
        getProjectPreferences().put(
                PreferenceConstants.PROXY_LAST_POJO_METHOD_NAME, getMethodName());
    }

    /**
     * {@inheritDoc} 
     */
    public boolean validateControls() {
        if (getClassName().length() == 0) {
            getWizardPage().updateStatus(Messages.invalid_target_pojo_class_name_msg);
            return false;
        }
        if (getMethodName().length() == 0) {
            getWizardPage().updateStatus(Messages.invalid_target_pojo_method_name_msg);
            return false;
        }
        return true;
    }

    /**
     * @return the target pojo class name
     */
    public final String getClassName() {
        return mClassNameText.getText();
    }

    /**
     * @param className target class name to set
     */
    public final void setClassName(
            final String className) {
        mClassNameText.setText(className);
    }

    /**
     * @return the target pojo method name
     */
    public final String getMethodName() {
        return mMethodNameText.getText();
    }

    /**
     * @param methodName target method name to set
     */
    public final void setMethodName(
            final String methodName) {
        mMethodNameText.setText(methodName);
    }
    
    /**
     * @return the target POJO parameters as a formatted POJO parameters object
     */
    public PojoParameters getPojoTargetParameters() {
        PojoParameters pojoParameters = new PojoParameters();
        pojoParameters.setClassName(getClassName());
        pojoParameters.setMethodName(getMethodName());
        return pojoParameters;
        
    }

}
