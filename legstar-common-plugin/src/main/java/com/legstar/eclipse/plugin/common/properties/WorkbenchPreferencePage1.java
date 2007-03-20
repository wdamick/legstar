/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.eclipse.plugin.common.properties;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.jface.preference.IPreferenceStore;
import com.legstar.eclipse.plugin.common.Activator;
import com.legstar.eclipse.plugin.common.LegstarConstants;


/**
 * A preference page to setup settings with influence over the binding
 * classes generation process.
 *
 */
public class WorkbenchPreferencePage1
	extends PreferencePage
	implements IWorkbenchPreferencePage,  SelectionListener {


	/** Ant default logger radio button. */
	private Button mRbAntDefaultLogger;

	/** Ant console logger radio button. */
	private Button mRbAntConsoleLogger;

	/** The selected Ant message level. */
	private Spinner mSpAntMessageLevel;
	
	/** The Ant group label. */
	private static final String ANT_GRP_LBL = "Ant options";

	/** The Ant default logger option label. */
	private static final String ANT_DFLT_LOG_LBL = "Use Ant Default logger";

	/** The Ant console logger option label. */
	private static final String ANT_CONSO_LOG_LBL = "Use Ant Console logger";

	/** The Ant message level label. */
	private static final String ANT_MSG_LVL_LBL =
		"Log messages with levels up to:";
	
	/** The message header for warning on ant console logger. */
	private static final String ANT_CONSO_WARN_HDR =
		"Console logger selected,";
	
	/** The warning message text on ant console logger. */
	private static final String ANT_CONSO_WARN_MSG =
	    "Warning: make sure to add com.legstar.eclipse.plugin.common_0.0.1.jar"
	    + " to the global entries in Windows-->Preferences-->"
	    + "Ant-->Runtime";

	/**
	 * Create a composite with a grid layout and given number of columns.
	 * @param parent the owner
	 * @param columns number of colums the layout should have
	 * @return the newly created composite
	 */
	private Composite createComposite(
			final Composite parent, final int columns) {
		Composite composite = new Composite(parent, SWT.NULL);
		GridLayout layout = new GridLayout();
		composite.setLayout(layout);
		layout.numColumns = columns;
		GridData data = new GridData(GridData.FILL);
		data.grabExcessHorizontalSpace = true;
		composite.setLayoutData(data);
		return composite;
	}

	/**
	 * Create a label with a given text.
	 * @param parent the owner
	 * @param text label text
	 * @return the newly created label
	 */
	private Label createLabel(
			final Composite parent, final String text) {
		Label label = new Label(parent, SWT.LEFT);
		label.setText(text);
		return label;
	}

	/**
	 * Create a group with a given text.
	 * @param parent the owner
	 * @param text label text
	 * @return the newly created group
	 */
	private Group createGroup(
			final Composite parent, final String text) {
		Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
		group.setText(text);
		GridLayout layout = new GridLayout();
		group.setLayout(layout);
		GridData data = new GridData(GridData.FILL);
		data.grabExcessHorizontalSpace = true;
		group.setLayoutData(data);
		return group;
	}

	/**
	 * Create a radio button with a given text.
	 * @param parent the owner
	 * @param label label text
	 * @return the newly created radio button
	 */
	private Button createRadioButton(
			final Composite parent, final String label) {
		Button button = new Button(parent, SWT.RADIO | SWT.LEFT);
		button.setText(label);
		button.addSelectionListener(this);
		return button;
	}

	/**
	 * Create a spinner with increment 1.
	 * @param parent the owner
	 * @param min the lower limit
	 * @param max the upper limit
	 * @return the newly created spinner control
	 */
	private Spinner createSpinner(
			final Composite parent, final int min, final int max) {
		Spinner spinner = new Spinner(parent, SWT.BORDER);
		spinner.setMinimum(min);
		spinner.setMaximum(max);
		spinner.setSelection(2);
		spinner.setIncrement(1);
		spinner.setPageIncrement(1);
		spinner.pack();
		spinner.addSelectionListener(this);
		return spinner;
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(
	 * org.eclipse.ui.IWorkbench)
	 * @param workbench pointer to workbench
	 */
	public final void init(final IWorkbench workbench) {
		initializeDefaultPreferences(getPreferenceStore());
	}

	/**
	 * @see org.eclipse.jface.preference.PreferencePage#doGetPreferenceStore()
	 * @return a preference store
	 */
	protected final IPreferenceStore doGetPreferenceStore() {
		return Activator.getDefault().getPreferenceStore();
	}

	/**
	 * Setup the reference store with default values.
	 * @param store the current preferences store
	 */
	private void initializeDefaultPreferences(final IPreferenceStore store) {
		store.setDefault(LegstarConstants.ANT_LOGGER_PREF,
				LegstarConstants.ANT_DEFAULT_LOGGER_CLASS);
		store.setDefault(LegstarConstants.ANT_MSGLVL_PREF,
				LegstarConstants.ANT_DEFAULT_MSGLVL);
	}

	/**
	 * @see org.eclipse.jface.preference.PreferencePage#createContents(
	 * org.eclipse.swt.widgets.Composite)
	 * @param parent the parent composite
	 * @return the new control
	 */
	@Override
	protected final Control createContents(final Composite parent) {

		//compositeTab1 << parent
		Composite compositeTab1 = createComposite(parent, 2);
		final Group antGroup = createGroup(compositeTab1, ANT_GRP_LBL);

		//composite_checkBox << composite_tab2
		Composite compositeCheckBox = createComposite(antGroup, 1);
		mRbAntDefaultLogger = createRadioButton(
				compositeCheckBox, ANT_DFLT_LOG_LBL); 
		mRbAntConsoleLogger = createRadioButton(
				compositeCheckBox, ANT_CONSO_LOG_LBL); 

		//compositeTab2 << antGroup
		Composite compositeTab2 = createComposite(antGroup, 2);
		createLabel(compositeTab2, ANT_MSG_LVL_LBL);
		mSpAntMessageLevel = createSpinner(compositeTab2, 1, 5);

		initializeValues();
		return compositeTab1;
	}

	/**
	 * Initialize widgets with values coming from the preference store.
	 */
	private void initializeValues()	{
		IPreferenceStore store = getPreferenceStore();

		if (store.getString(LegstarConstants.ANT_LOGGER_PREF).compareTo(
				LegstarConstants.ANT_CONSOLE_LOGGER_CLASS) == 0) {
			mRbAntConsoleLogger.setSelection(true);
			mRbAntDefaultLogger.setSelection(false);
		} else {
			mRbAntConsoleLogger.setSelection(false);
			mRbAntDefaultLogger.setSelection(true);
		}
		mSpAntMessageLevel.setSelection(store.getInt(
				LegstarConstants.ANT_MSGLVL_PREF));

	}

	/**
	 * Method declared on PreferencePage.
	 * @return true if OK confirmed
	 */
	public final boolean performOk() {
		storeValues();
		Activator.getDefault().savePluginPreferences();
		return true;
	}

	/**
	 * Method declared on PreferencePage.
	 */
	protected final void performDefaults() {
		super.performDefaults();
		initializeDefaults();

	}
	/**
	 * Save the preference values in the preferences store.
	 */
	private void storeValues() {
		IPreferenceStore store = getPreferenceStore();

		if (mRbAntConsoleLogger.getSelection()) {
			store.setValue(LegstarConstants.ANT_LOGGER_PREF,
					LegstarConstants.ANT_CONSOLE_LOGGER_CLASS);
		} else {
			store.setValue(LegstarConstants.ANT_LOGGER_PREF,
					LegstarConstants.ANT_DEFAULT_LOGGER_CLASS);
		}
		store.setValue(LegstarConstants.ANT_MSGLVL_PREF,
				mSpAntMessageLevel.getSelection());
	}

	/**
	 * Reinitializes values from default settings.
	 */
	private void initializeDefaults() {
		IPreferenceStore store = getPreferenceStore();
		if (store.getDefaultString(LegstarConstants.ANT_LOGGER_PREF).compareTo(
				LegstarConstants.ANT_CONSOLE_LOGGER_CLASS) == 0) {
			mRbAntConsoleLogger.setSelection(true);
			mRbAntDefaultLogger.setSelection(false);
		} else {
			mRbAntConsoleLogger.setSelection(false);
			mRbAntDefaultLogger.setSelection(true);
		}
		mSpAntMessageLevel.setSelection(store.getDefaultInt(
				LegstarConstants.ANT_MSGLVL_PREF));

		storeValues();
	}
	
	/**
	 * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(
	 * org.eclipse.swt.events.SelectionEvent)
	 * @param e selection event
	 */
	public final void widgetDefaultSelected(final SelectionEvent e) {

	}

	/**
	 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(
	 * org.eclipse.swt.events.SelectionEvent)
	 * @param e selection event
	 */
	public final void widgetSelected(final SelectionEvent e) {
		if (e.getSource().equals(mRbAntConsoleLogger)) {
			if (mRbAntConsoleLogger.getSelection()) {
				MessageDialog.openWarning(null,
						ANT_CONSO_WARN_HDR,
						ANT_CONSO_WARN_MSG);
			}
			
		}
	}

}
