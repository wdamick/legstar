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
package com.legstar.eclipse.plugin.coxbgen.properties;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.jface.preference.IPreferenceStore;
import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.eclipse.plugin.coxbgen.CoxbgenPreferences;

/**
 * A preference page to setup settings for COXB generation.
 *
 */
public class WorkbenchPreferencePage1
	extends PreferencePage
	implements IWorkbenchPreferencePage,  SelectionListener {

	/** The group label text. */
	public static final String GROUP_LABEL_TEXT = "COXB Generator";
	
	/** The COXB generator location label text. */
	public static final String GEN_LOC_LABEL_TEXT =
		"Location:";
	
	/** Field used to locate the COXB generator. */
	private DirectoryFieldEditor mCoxbgenLocator;
	
	/** DirectoryFieldEditor widget name. */
	private static final String GEN_LOC_NAME = "CoxbgenLocation";
	
	/** File used to probe the coxbgen install. */
	private static final String GEN_LOC_PROBE = "ant/build-jaxb.xml";
	
	/** Location does not exist error message. */
	private static final String GEN_LOC_DOES_NOT_EXIST =
		"COXB Generator location does not exist";
	
	/** Location not a folder error message. */
	private static final String GEN_LOC_NOT_A_FOLDER =
		"COXB Generator location is not a folder";
	
	/** Location not a valid coxbgen install error message. */
	private static final String GEN_LOC_NOT_VALID =
		"COXB Generator location is not a valid Coxbgen install folder";
	
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
	 * Create a group with a given text.
	 * @param parent the owner
	 * @param text label text
	 * @param columns number of colums the layout should have
	 * @return the newly created group
	 */
	private Group createGroup(
			final Composite parent, final String text, final int columns) {
		Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
		group.setText(text);
		GridLayout layout = new GridLayout();
		group.setLayout(layout);
		layout.numColumns = columns;
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		data.grabExcessHorizontalSpace = true;
		group.setLayoutData(data);
		return group;
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
		store.setDefault(
				CoxbgenPreferences.GEN_LOC_PREF,
				CoxbgenPreferences.DEFAULT_GEN_LOC);
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
		final Group group = createGroup(compositeTab1, GROUP_LABEL_TEXT, 2);

		mCoxbgenLocator = new DirectoryFieldEditor(GEN_LOC_NAME,
				GEN_LOC_LABEL_TEXT, group); 
		mCoxbgenLocator.getTextControl(group).addModifyListener(
				new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				setValid(checkValues());
			}
		});
		
		group.moveAbove(null);

		initializeValues();
		return compositeTab1;
	}

	/**
	 * Initialize widgets with values coming from the preference store.
	 */
	private void initializeValues()	{
		IPreferenceStore store = getPreferenceStore();

		mCoxbgenLocator.setStringValue(store.getString(
				CoxbgenPreferences.GEN_LOC_PREF));

	}

	/**
	 * Method declared on PreferencePage.
	 * @return true if OK confirmed
	 */
	public final boolean performOk() {
		if (!checkValues()) {
			setValid(false);
			return false;
		}
		storeValues();
		Activator.getDefault().savePluginPreferences();
		return true;
	}
	
	/**
	 * Check that the location entered is correct.
	 * @return the result of the verification
	 */
	private boolean checkValues() {
		setErrorMessage(null);
		String cl = mCoxbgenLocator.getStringValue();
		File f = new File(cl);
		if (!f.exists()) {
			setErrorMessage(GEN_LOC_DOES_NOT_EXIST);
			return false;
		}
		if (!f.isDirectory()) {
			setErrorMessage(GEN_LOC_NOT_A_FOLDER);
			return false;
		}
		File probe = new File(cl + '/' + GEN_LOC_PROBE);
		if (!probe.exists()) {
			setErrorMessage(GEN_LOC_NOT_VALID);
			return false;
		}
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

		store.setValue(CoxbgenPreferences.GEN_LOC_PREF,
				mCoxbgenLocator.getStringValue());
	}

	/**
	 * Reinitializes values from default settings.
	 */
	private void initializeDefaults() {
		IPreferenceStore store = getPreferenceStore();
		mCoxbgenLocator.setStringValue(store.getDefaultString(
				CoxbgenPreferences.GEN_LOC_PREF));
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
	}

}
