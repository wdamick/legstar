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
package com.legstar.eclipse.plugin.jaxwsgen.preferences;

import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

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

public class JaxwsCixsPreferencePage extends FieldEditorPreferencePage
implements IWorkbenchPreferencePage {

    /**
     * Constructs a preference page.
     */
    public JaxwsCixsPreferencePage() {
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

        addField(new DirectoryFieldEditor(
                PreferenceConstants.DEFAULT_J2EE_WAR_FOLDER,
                Messages.preference_war_location_label + ':',
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.DEFAULT_J2EE_WDD_FOLDER,
                Messages.preference_wdd_folder_label + ':',
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.COBOL_SAMPLE_FOLDER,
                Messages.preference_cobol_folder_label + ':',
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.ADAPTER_WSDL_TARGET_NAMESPACE_PREFIX,
                Messages.preference_wsdl_target_namespace_prefix_label + ':',
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.ADAPTER_WSDL_SERVICE_NAME_SUFFIX,
                Messages.preference_wsdl_service_name_suffix_label + ':',
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.ADAPTER_WSDL_PORT_NAME_SUFFIX,
                Messages.preference_wsdl_port_name_suffix_label + ':',
                getFieldEditorParent()));

        /* For now, the only scheme supported by clients is http */
//        addField(new StringFieldEditor(
//                PreferenceConstants.PROXY_HTTP_SCHEME,
//                Messages.preference_proxy_http_scheme_label + ':',
//                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.PROXY_DEFAULT_HTTP_HOST,
                Messages.preference_proxy_http_host_label + ':',
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.PROXY_DEFAULT_HTTP_PORT,
                Messages.preference_proxy_http_port_label + ':',
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.PROXY_HTTP_PATH_TEMPLATE,
                Messages.preference_proxy_http_path_template_label + ':',
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
