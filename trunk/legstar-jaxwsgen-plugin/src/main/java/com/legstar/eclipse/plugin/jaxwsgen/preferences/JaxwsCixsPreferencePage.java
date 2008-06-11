package com.legstar.eclipse.plugin.jaxwsgen.preferences;

import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

import com.legstar.eclipse.plugin.jaxwsgen.Activator;


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

public class JaxwsCixsPreferencePage extends FieldEditorPreferencePage
		implements IWorkbenchPreferencePage {

	/**
	 * Constructs a preference page.
	 */
	public JaxwsCixsPreferencePage() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("Setup preferences for Jaxws Cixs generators");
	}

	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {

        addField(new DirectoryFieldEditor(
                PreferenceConstants.J2EE_INSTALL_FOLDER,
                "J2EE install folder:", getFieldEditorParent()));

        addField(new DirectoryFieldEditor(
                PreferenceConstants.J2EE_WAR_FOLDER,
                "J2EE deployment folder:", getFieldEditorParent()));
 
        addField(new StringFieldEditor(
                PreferenceConstants.J2EE_WDD_FOLDER,
                "Web descriptors folder:", getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.WS_TARGET_NAMESPACE_PREFIX,
                "Web Services namespace prefix:", getFieldEditorParent()));

	}

	/**
	 * {@inheritDoc}
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(
	 * org.eclipse.ui.IWorkbench)
	 */
	public void init(final IWorkbench workbench) {
	}

}
