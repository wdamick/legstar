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
package com.legstar.eclipse.plugin.jaxwsgen.preferences;

import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;

import com.legstar.eclipse.plugin.cixscom.preferences.AbstractCixsPreferencePage;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;


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

public class JaxwsCixsPreferencePage extends AbstractCixsPreferencePage {

    /**
     * Constructs a preference page.
     */
    public JaxwsCixsPreferencePage() {
        super();
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

        super.createFieldEditors();
        
        addField(new StringFieldEditor(
                PreferenceConstants.DEFAULT_J2EE_WDD_FOLDER,
                Messages.preference_wdd_folder_label + ':',
                getFieldEditorParent()));

        addField(new DirectoryFieldEditor(
                PreferenceConstants.DEFAULT_J2EE_WAR_FOLDER,
                Messages.preference_war_location_label + ':',
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
