/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;

/**
 * Abstract page. Collects parameters needed for common Schemagen
 * wizard pages. Each subclass will ad its own widgets.
 * 
 */
public abstract class AbstractToXsdWizardPage extends AbstractWizardPage {

    /**
     * Construct the page.
     * 
     * @param pageName the page name
     * @param pageTitle the page title
     * @param pageDesc the page description
     * @param selection the current workbench selection
     */
    protected AbstractToXsdWizardPage(
            final IStructuredSelection selection,
            final String pageName,
            final String pageTitle,
            final String pageDesc) {
        super(selection, pageName, pageTitle, pageDesc);
    }

    /**
     * Creates a combo box with the given items spanning all columns.
     * 
     * @param container the parent container
     * @param items a list of items
     * @return the combo box
     */
    protected Combo createComboFromItemsArray(
            final Composite container, final String[] items) {
        final Combo combo = new Combo(container, SWT.READ_ONLY);
        combo.setItems(items);
        combo.select(0);
        combo.addSelectionListener(new SelectionListener() {

            /** {@inheritDoc} */
            public void widgetDefaultSelected(final SelectionEvent arg0) {
                dialogChanged();
            }

            /** {@inheritDoc} */
            public void widgetSelected(final SelectionEvent arg0) {
                dialogChanged();
            }

        });

        final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.horizontalSpan = LAYOUT_COLUMNS;
        combo.setLayoutData(gridData);
        return combo;
    }

    /**
     * Create a multi line text field that fills a grid column.
     * 
     * @param container the parent container
     * @param span how many columns of the grid this should span
     * @return the new text field
     */
    protected Text createMultilineTextField(
            final Composite container, final int span) {
        final Text text = new Text(container,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        final GridData gridData = new GridData(GridData.FILL_BOTH);
        gridData.horizontalSpan = span;
        text.setLayoutData(gridData);
        text.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        return text;
    }

    /**
     * {@inheritDoc} The reason we override this is that is the last chance to
     * set the
     * finish button status correctly. The problem is that the finish button
     * status is stored at the wizard and shared by all pages. Therefore
     * is one page is in error, even if that page ended up not being
     * selected, the finish button is not enabled.
     * Here we know we are switching pages so we make sure that the finish
     * button reflects the status of the target page.
     */
    @Override
    public void setPreviousPage(final IWizardPage page) {
        super.setPreviousPage(page);
        this.dialogChanged();
    }

    /**
     * Project-related content can be initialized only when the project
     * has been identified. It is the responsibility of the previous wizard
     * page to identify a project in the Eclipse workspace and then call
     * this method.
     */
    public abstract void initProjectContent();

    /**
     * @return the set of properties that need to be persisted
     */
    public abstract Properties getPersistProperties();

    /**
     * @return our Wizard type
     */
    public MainWizard getWizard() {
        return (MainWizard) super.getWizard();
    }

    /**
     * @return the Eclipse Project to which artifacts are attached
     */
    public IProject getProject() {
        return getWizard().getProject();
    }

    /**
     * @param project the Eclipse Project to which artifacts are attached to set
     */
    public void setProject(final IProject project) {
        getWizard().setProject(project);
    }

    /**
     * @return the project scope preferences
     */
    public IEclipsePreferences getProjectPreferences() {
        return getWizard().getProjectPreferences();
    }

    /**
     * @return the default scope preferences
     */
    public IPreferenceStore getDefaultPreferences() {
        return getWizard().getDefaultPreferences();
    }

}
