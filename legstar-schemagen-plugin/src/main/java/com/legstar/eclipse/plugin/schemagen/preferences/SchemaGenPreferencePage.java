/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.schemagen.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.legstar.cob2xsd.Cob2XsdContext.CodeFormat;
import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.Messages;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class SchemaGenPreferencePage extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    /**
     * Constructs a preference page.
     */
    public SchemaGenPreferencePage() {
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

        /*
         * -------------------------------------------------------------------
         * COBOL source format related options
         */
        addField(new RadioGroupFieldEditor(
                PreferenceConstants.DEFAULT_CODE_FORMAT,
                Messages.preference_default_code_format_label,
                2,
                new String[][] {
                        { Messages.fixed_format_label,
                                CodeFormat.FIXED_FORMAT.toString() },
                        {
                                Messages.free_format_label,
                                CodeFormat.FREE_FORMAT.toString() }
                },
                getFieldEditorParent()));

        addField(new IntegerFieldEditor(
                PreferenceConstants.DEFAULT_START_COLUMN,
                Messages.preference_default_start_column_label + ':',
                getFieldEditorParent()));

        addField(new IntegerFieldEditor(
                PreferenceConstants.DEFAULT_END_COLUMN,
                Messages.preference_default_end_column_label + ':',
                getFieldEditorParent()));

        /*
         * -------------------------------------------------------------------
         * XML Schema related default options
         */
        addField(new StringFieldEditor(
                PreferenceConstants.XSD_NAMESPACE_PREFIX,
                Messages.preference_xsd_namespace_prefix_label + ':',
                getFieldEditorParent()));
        addField(new StringFieldEditor(
                PreferenceConstants.DEFAULT_XSD_ENCODING,
                Messages.preference_default_xsd_encoding_label + ':',
                getFieldEditorParent()));
        addField(new BooleanFieldEditor(
                PreferenceConstants.DEFAULT_XSD_MAP_CONDITIONS_TO_FACETS,
                Messages.preference_default_map_conditions_to_facets_label,
                getFieldEditorParent()));
        addField(new StringFieldEditor(
                PreferenceConstants.DEFAULT_XSD_CUSTOM_XSLT_FILE_NAME,
                Messages.preference_default_custom_xslt_file_name_label + ':',
                getFieldEditorParent()));
        addField(new BooleanFieldEditor(
                PreferenceConstants.DEFAULT_XSD_NAME_CONFLICT_PREPEND_PARENT_NAME,
                Messages.preference_default_name_conflict_prepend_parent_name_label,
                getFieldEditorParent()));
        addField(new BooleanFieldEditor(
                PreferenceConstants.DEFAULT_XSD_ELEMENT_NAMES_START_WITH_UPPERCASE,
                Messages.preference_default_element_names_start_with_uppercase_label,
                getFieldEditorParent()));

        /*
         * -------------------------------------------------------------------
         * LegStar annotations default options
         */
        addField(new BooleanFieldEditor(
                PreferenceConstants.DEFAULT_ADD_LEGSTAR_ANNOTATIONS,
                Messages.preference_default_add_legstar_annotations_label,
                getFieldEditorParent()));

        /*
         * -------------------------------------------------------------------
         * COBOL compiler related options
         */
        addField(new StringFieldEditor(
                PreferenceConstants.DEFAULT_CURRENCY_SIGN,
                Messages.preference_default_currency_sign_label + ':',
                getFieldEditorParent()));
        addField(new StringFieldEditor(
                PreferenceConstants.DEFAULT_CURRENCY_SYMBOL,
                Messages.preference_default_currency_symbol_label + ':',
                getFieldEditorParent()));
        addField(new BooleanFieldEditor(
                PreferenceConstants.DEFAULT_DECIMAL_POINT_IS_COMMA,
                Messages.preference_default_decimal_point_is_comma_label,
                getFieldEditorParent()));
        addField(new BooleanFieldEditor(
                PreferenceConstants.DEFAULT_NSYMBOL_DBCS,
                Messages.preference_default_nsymbol_dbcs_label,
                getFieldEditorParent()));
        addField(new BooleanFieldEditor(
                PreferenceConstants.DEFAULT_QUOTE_IS_QUOTE,
                Messages.preference_default_quote_is_quote_label,
                getFieldEditorParent()));

    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(final IWorkbench workbench) {
    }

}
