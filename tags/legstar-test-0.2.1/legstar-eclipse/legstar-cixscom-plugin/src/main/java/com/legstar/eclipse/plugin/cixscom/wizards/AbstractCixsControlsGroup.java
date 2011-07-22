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
package com.legstar.eclipse.plugin.cixscom.wizards;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;

/**
 * Holds a group of controls that might be visible or not.
 * <p/>
 * Such group of controls has a radio button which selection drives the
 * visibility of the entire group.
 * 
 */
public abstract class AbstractCixsControlsGroup {

    /** The wizard page to which this widget contributes. */
    private AbstractCixsGeneratorWizardPage _wizardPage;

    /** Button which selection drives the visibility of this group. */
    private Button _button = null;

    /** The group widget holding the controls. */
    private Group _group = null;

    /** Determines if this group is selected. */
    private boolean _initialSelection;

    /**
     * Construct this control group attaching it to a wizard page.
     * 
     * @param wizardPage the parent wizard page
     * @param selected whether this group should initially be selected
     */
    public AbstractCixsControlsGroup(
            final AbstractCixsGeneratorWizardPage wizardPage,
            final boolean selected) {
        _wizardPage = wizardPage;
        _initialSelection = selected;
    }

    /**
     * Contribute the radio button that allow selecting this proxy target.
     * 
     * @param composite the composite to contribute widgets to
     * @param buttonText the button text
     */
    public void createButton(final Composite composite, final String buttonText) {
        _button = new Button(composite, SWT.RADIO);
        _button.setText(buttonText);
    }

    /**
     * Contribute this widget holder widgets to a composite.
     * 
     * @param composite the composite to contribute widgets to
     * @param groupText the group label
     * @param columns the number of columns in the group layout
     */
    public void createControls(final Composite composite,
            final String groupText, final int columns) {
        _group = AbstractWizardPage.createGroup(composite, groupText, columns);
    }

    /**
     * A particular group might be visible of hidden behind some other group,
     * depending on the associated button selection status.
     */
    public void setVisibility() {
        getGroup().setVisible(getSelection());
        ((GridData) getGroup().getLayoutData()).exclude = !getSelection();
    }

    /**
     * @return true if the associated button is selected
     */
    public boolean getSelection() {
        return _button.getSelection();
    }

    /**
     * Creation of listeners on controls is separated from control creation.
     * This allows listeners to be created after all fields have been
     * initialized
     * and avoid triggering dialogChanged on initialization.
     */
    public void createListeners() {
        getButton().addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        createExtendedListeners();
    }

    /**
     * Initialize control values. This involves setting default values or
     * retrieving last saved values.
     */
    public void initControls() {
        getButton().setSelection(_initialSelection);
        initExtendedControls();
    }

    /**
     * Store the selected values in the project scoped preference store.
     */
    public void updateGenModel() {
        updateGenModelExtended();
    }

    /**
     * Contribute the radio button that allow selecting this proxy target.
     * 
     * @param composite the composite to contribute widgets to
     */
    public abstract void createButton(final Composite composite);

    /**
     * Contribute this widget holder widgets to a composite.
     * 
     * @param composite the composite to contribute widgets to
     */
    public abstract void createControls(final Composite composite);

    /**
     * Initialize control values. This involves setting default values or
     * retrieving last saved values.
     */
    public abstract void initExtendedControls();

    /**
     * Creation of listeners on controls is separated from control creation.
     * This allows listeners to be created after all fields have been
     * initialized
     * and avoid triggering dialogChanged on initialization.
     */
    public abstract void createExtendedListeners();

    /**
     * Store the selected values in the project scoped preference store.
     */
    public abstract void updateGenModelExtended();

    /**
     * Check controls for validity.
     * 
     * @return true if controls content is valid
     */
    public abstract boolean validateControls();

    /**
     * @return the The wizard page to which this widget contributes
     */
    public AbstractCixsGeneratorWizardPage getWizardPage() {
        return _wizardPage;
    }

    /**
     * @param wizardPage the The wizard page to which this widget contributes to
     *            set
     */
    public void setWizardPage(final AbstractCixsGeneratorWizardPage wizardPage) {
        _wizardPage = wizardPage;
    }

    /**
     * @return the project scope preferences
     */
    public IEclipsePreferences getProjectPreferences() {
        return getWizardPage().getProjectPreferences();
    }

    /**
     * @return the shell of the associated wisard page
     */
    public Shell getShell() {
        return getWizardPage().getShell();
    }

    /**
     * @return the button which selection drives the visibility of this group
     */
    public Button getButton() {
        return _button;
    }

    /**
     * @param button button which selection drives the visibility of this group
     *            to set
     */
    public void setButton(final Button button) {
        _button = button;
    }

    /**
     * @return the group widget holding the controls
     */
    public Group getGroup() {
        return _group;
    }

    /**
     * @param group the group widget holding the controls to set
     */
    public void setGroup(final Group group) {
        _group = group;
    }

    /**
     * @return whether this group is selected or not
     */
    public boolean isSelected() {
        return _button.getSelection();
    }
}
