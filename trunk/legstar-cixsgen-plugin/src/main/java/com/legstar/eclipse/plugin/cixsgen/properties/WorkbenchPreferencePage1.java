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
package com.legstar.eclipse.plugin.cixsgen.properties;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
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
import com.legstar.eclipse.plugin.cixsgen.Activator;
import com.legstar.eclipse.plugin.cixsgen.CixsgenPreferences;

/**
 * A preference page to setup settings for CIXS generation.
 *
 */
public class WorkbenchPreferencePage1
	extends PreferencePage
	implements IWorkbenchPreferencePage,  SelectionListener {

	/* ============================= LABELS SECTION =========================*/
	
	/** The base generator group label text. */
	public static final String GROUP1_LABEL_TEXT = "CIXS Generator";
	
	/** The CIXS Generator location label text. */
	public static final String GEN_LOC_LABEL_TEXT =
		"Location:";
	
	/** The cixs plugin generator group label text. */
	public static final String GROUP2_LABEL_TEXT = "Web Service Endpoints";
	
	/** The package name prefix label text. */
	public static final String CIXS_PKG_PFX_LABEL_TEXT =
		"Package name prefix:";
	
	/** The namespace prefix label text. */
	public static final String CIXS_NS_PFX_LABEL_TEXT =
		"Target namespace prefix:";
	
	/** The Ant scripts target location label text. */
	public static final String CIXS_ANT_DIR_LABEL_TEXT =
		"Ant scripts location:";
	
	/** The Web descriptors target location label text. */
	public static final String CIXS_WDD_DIR_LABEL_TEXT =
		"Web descriptors location:";
	
	/** The Properties files target location label text. */
	public static final String CIXS_PROP_DIR_LABEL_TEXT =
		"Properties files location:";
	
	/** The War files target location label text. */
	public static final String CIXS_WAR_DIR_LABEL_TEXT =
		"War files location:";
	
	/* ============================ WIDGETS SECTION =========================*/
	
	/** Field used to locate the CIXS generator. */
	private DirectoryFieldEditor mCixsgenLocator;
	
	/** CIXS package name prefix. */
	private Text mTxCixsPackagePrefix;
	
	/** CIXS namespace prefix. */
	private Text mTxCixsNamespacePrefix;

	/** CIXS Ant scripts target location. */
	private Text mTxCixsAntDir;

	/** CIXS Web descriptors target location. */
	private Text mTxCixsWddDir;

	/** CIXS Properties files target location. */
	private Text mTxCixsPropDir;

	/** CIXS War files target location. */
	private Text mTxCixsWarDir;

	/* ======================= ERROR MESSAGE TEXT SECTION ===================*/
	
	/** Empy package name prefix error message. */
	private static final String PKG_NAME_EMPTY =
		"You must specify a package name prefix";
	
	/** Empy namespace prefix error message. */
	private static final String NS_EMPTY =
		"You must specify a namespace prefix";

	/** Location does not exist error message. */
	private static final String GEN_LOC_DOES_NOT_EXIST =
		"CIXS Generator location does not exist";
	
	/** Location not a folder error message. */
	private static final String GEN_LOC_NOT_A_FOLDER =
		"CIXS Generator location is not a folder";
	
	/** Location not a valid Cixsgen install error message. */
	private static final String GEN_LOC_NOT_VALID =
		"CIXS Generator location is not a valid Cixsgen install folder";
	
	/* =========================== RESOURCES SECTION ========================*/
	
	/** DirectoryFieldEditor widget name. */
	private static final String GEN_LOC_NAME = "CixsgenLocation";
	
	/** File used to probe the cixsgen install. */
	private static final String GEN_LOC_PROBE = "ant/build-service.xml";
	
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
	 * Create a Text field with a given text.
	 * @param parent the owner
	 * @param label label text
	 * @return the newly created Text field
	 */
	private Text createText(
			final Composite parent, final String label) {
		Text text = new Text(parent, SWT.BORDER | SWT.SINGLE);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		text.setLayoutData(gd);
		text.setEditable(true);
		text.setText(label);
		text.addSelectionListener(this);
		return text;
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
				CixsgenPreferences.GEN_LOC_PREF,
				CixsgenPreferences.DEFAULT_GEN_LOC);
		store.setDefault(
				CixsgenPreferences.CIXS_PKG_PFX_PREF,
				CixsgenPreferences.DEFAULT_CIXS_PKG_PFX);
		store.setDefault(
				CixsgenPreferences.CIXS_NS_PFX_PREF,
				CixsgenPreferences.DEFAULT_CIXS_NS_PFX);
		store.setDefault(
				CixsgenPreferences.CIXS_ANT_DIR_PREF,
				CixsgenPreferences.DEFAULT_CIXS_ANT_DIR);
		store.setDefault(
				CixsgenPreferences.CIXS_WDD_DIR_PREF,
				CixsgenPreferences.DEFAULT_CIXS_WDD_DIR);
		store.setDefault(
				CixsgenPreferences.CIXS_PROP_DIR_PREF,
				CixsgenPreferences.DEFAULT_CIXS_PROP_DIR);
		store.setDefault(
				CixsgenPreferences.CIXS_WAR_DIR_PREF,
				CixsgenPreferences.DEFAULT_CIXS_WAR_DIR);
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
		Composite compositeTab1 = createComposite(parent, 1);
		final Group group1 = createGroup(compositeTab1, GROUP1_LABEL_TEXT, 2);

		mCixsgenLocator = new DirectoryFieldEditor(GEN_LOC_NAME,
				GEN_LOC_LABEL_TEXT, group1); 
		mCixsgenLocator.getTextControl(group1).addModifyListener(
				new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				setValid(checkValues());
			}
		});
		
		final Group group2 = createGroup(compositeTab1, GROUP2_LABEL_TEXT, 2);

		createLabel(group2, CIXS_PKG_PFX_LABEL_TEXT);
		mTxCixsPackagePrefix = createText(group2,
				CixsgenPreferences.DEFAULT_CIXS_PKG_PFX); 
		mTxCixsPackagePrefix.addModifyListener(
				new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				setValid(checkValues());
			}
		});

		createLabel(group2, CIXS_NS_PFX_LABEL_TEXT);
		mTxCixsNamespacePrefix = createText(group2,
				CixsgenPreferences.DEFAULT_CIXS_NS_PFX); 
		mTxCixsNamespacePrefix.addModifyListener(
				new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				setValid(checkValues());
			}
		});

		createLabel(group2, CIXS_ANT_DIR_LABEL_TEXT);
		mTxCixsAntDir = createText(group2,
				CixsgenPreferences.DEFAULT_CIXS_ANT_DIR); 
		
		createLabel(group2, CIXS_WDD_DIR_LABEL_TEXT);
		mTxCixsWddDir = createText(group2,
				CixsgenPreferences.DEFAULT_CIXS_WDD_DIR); 
		
		createLabel(group2, CIXS_PROP_DIR_LABEL_TEXT);
		mTxCixsPropDir = createText(group2,
				CixsgenPreferences.DEFAULT_CIXS_PROP_DIR); 
		
		createLabel(group2, CIXS_WAR_DIR_LABEL_TEXT);
		mTxCixsWarDir = createText(group2,
				CixsgenPreferences.DEFAULT_CIXS_WAR_DIR); 
		
		initializeValues();
		return compositeTab1;
	}

	/**
	 * Initialize widgets with values coming from the preference store.
	 */
	private void initializeValues()	{
		IPreferenceStore store = getPreferenceStore();

		mCixsgenLocator.setStringValue(store.getString(
				CixsgenPreferences.GEN_LOC_PREF));
		mTxCixsPackagePrefix.setText(store.getString(
				CixsgenPreferences.CIXS_PKG_PFX_PREF));
		mTxCixsNamespacePrefix.setText(store.getString(
				CixsgenPreferences.CIXS_NS_PFX_PREF));
		mTxCixsAntDir.setText(store.getString(
				CixsgenPreferences.CIXS_ANT_DIR_PREF));
		mTxCixsWddDir.setText(store.getString(
				CixsgenPreferences.CIXS_WDD_DIR_PREF));
		mTxCixsPropDir.setText(store.getString(
				CixsgenPreferences.CIXS_PROP_DIR_PREF));
		mTxCixsWarDir.setText(store.getString(
				CixsgenPreferences.CIXS_WAR_DIR_PREF));

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
	 * Check that data entered is correct.
	 * @return the result of the verification
	 */
	private boolean checkValues() {
		setErrorMessage(null);
		String cl = mCixsgenLocator.getStringValue();
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
		if (mTxCixsPackagePrefix.getText() == null
				|| mTxCixsPackagePrefix.getText().trim().length() < 1) {
			setErrorMessage(PKG_NAME_EMPTY);
			return false;
		}
		if (mTxCixsNamespacePrefix.getText() == null
				|| mTxCixsNamespacePrefix.getText().trim().length() < 1) {
			setErrorMessage(NS_EMPTY);
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

		store.setValue(CixsgenPreferences.GEN_LOC_PREF,
				mCixsgenLocator.getStringValue());
		store.setValue(CixsgenPreferences.CIXS_PKG_PFX_PREF,
				mTxCixsPackagePrefix.getText());
		store.setValue(CixsgenPreferences.CIXS_NS_PFX_PREF,
				mTxCixsNamespacePrefix.getText());
		store.setValue(CixsgenPreferences.CIXS_ANT_DIR_PREF,
				mTxCixsAntDir.getText());
		store.setValue(CixsgenPreferences.CIXS_WDD_DIR_PREF,
				mTxCixsWddDir.getText());
		store.setValue(CixsgenPreferences.CIXS_PROP_DIR_PREF,
				mTxCixsPropDir.getText());
		store.setValue(CixsgenPreferences.CIXS_WAR_DIR_PREF,
				mTxCixsWarDir.getText());
	}

	/**
	 * Reinitializes values from default settings.
	 */
	private void initializeDefaults() {
		IPreferenceStore store = getPreferenceStore();
		mCixsgenLocator.setStringValue(store.getDefaultString(
				CixsgenPreferences.GEN_LOC_PREF));
		mTxCixsPackagePrefix.setText(store.getDefaultString(
				CixsgenPreferences.CIXS_PKG_PFX_PREF));
		mTxCixsNamespacePrefix.setText(store.getDefaultString(
				CixsgenPreferences.CIXS_NS_PFX_PREF));
		mTxCixsAntDir.setText(store.getDefaultString(
				CixsgenPreferences.CIXS_ANT_DIR_PREF));
		mTxCixsWddDir.setText(store.getDefaultString(
				CixsgenPreferences.CIXS_WDD_DIR_PREF));
		mTxCixsPropDir.setText(store.getDefaultString(
				CixsgenPreferences.CIXS_PROP_DIR_PREF));
		mTxCixsWarDir.setText(store.getDefaultString(
				CixsgenPreferences.CIXS_WAR_DIR_PREF));
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
