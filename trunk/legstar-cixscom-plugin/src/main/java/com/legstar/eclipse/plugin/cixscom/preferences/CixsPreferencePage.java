package com.legstar.eclipse.plugin.cixscom.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

import com.legstar.eclipse.plugin.cixscom.Activator;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class CixsPreferencePage extends FieldEditorPreferencePage
		implements IWorkbenchPreferencePage {

	/**
	 * Constructs a preference page.
	 */
	public CixsPreferencePage() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("Setup preferences for Cixs generators");
	}

	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {

        addField(new StringFieldEditor(
				PreferenceConstants.CIXS_PACKAGE_NAME_PREFIX,
				"Package name prefix:", getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.CIXS_TARGET_ANT_FOLDER,
                "Ant scripts folder:", getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.CIXS_TARGET_PROP_FOLDER,
                "Properties folder:", getFieldEditorParent()));

	}

	/**
	 * {@inheritDoc}
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(
	 * org.eclipse.ui.IWorkbench)
	 */
	public void init(final IWorkbench workbench) {
	}

}
