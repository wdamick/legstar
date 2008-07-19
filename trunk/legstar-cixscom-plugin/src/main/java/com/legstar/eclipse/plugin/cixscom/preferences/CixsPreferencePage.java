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
package com.legstar.eclipse.plugin.cixscom.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

import com.legstar.eclipse.plugin.cixscom.Activator;
import com.legstar.eclipse.plugin.cixscom.Messages;

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
		setDescription(Messages.preference_page_description);
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
				Messages.cixs_package_name_prefix_label + ':',
				getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.CIXS_TARGET_PROP_FOLDER,
                Messages.cixs_properties_folder_label + ':',
                getFieldEditorParent()));

	}

	/**
	 * {@inheritDoc}
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(
	 * org.eclipse.ui.IWorkbench)
	 */
	public void init(final IWorkbench workbench) {
	}

}
