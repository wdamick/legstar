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
package com.legstar.eclipse.plugin.schemagen;

import org.eclipse.osgi.util.NLS;

/**
 * This plugin localized message class.
 */
public class Messages extends NLS {

    /** All messages come from this property file. */
    private static final String BUNDLE_NAME =
            "com.legstar.eclipse.plugin.schemagen.messages";

    /** New file action name. */
    public static String new_file_title;
    /** The wizard icon. */
    public static String data_structure_mapping_icon;
    /** Menu option label. */
    public static String action_label;
    /** Toolbar button tooltip. */
    public static String action_tooltip;
    /** The preference page title. */
    public static String schemagen_preference_page_title;
    /** Description on preference page. */
    public static String preference_page_description;

    /*
     * -------------------------------------------------------------------
     * COBOL source files encoding
     */

    /** COBOL files encoding label. */
    public static String cobol_files_encoding_label;

    /*
     * -------------------------------------------------------------------
     * COBOL source format related options
     */

    /** Default code formatting label. */
    public static String preference_default_code_format_label;

    /** Fixed format label. */
    public static String fixed_format_label;

    /** Free format label. */
    public static String free_format_label;

    /** Default starting column label. */
    public static String preference_default_start_column_label;

    /** Default ending column label. */
    public static String preference_default_end_column_label;
    /*
     * -------------------------------------------------------------------
     * XML Schema related default options
     */
    /** XML Schema namespace prefix label. */
    public static String preference_xsd_namespace_prefix_label;

    /** XML schema default encoding. */
    public static String preference_default_xsd_encoding_label;

    /** XML schema default mapping of 88 conditions to facets. */
    public static String preference_default_map_conditions_to_facets_label;

    /** XML schema default customization XSLT file name. */
    public static String preference_default_custom_xslt_file_name_label;

    /** XML schema default prepending parent name if name conflict. */
    public static String preference_default_name_conflict_prepend_parent_name_label;

    /** XML schema default element name starting with uppercase. */
    public static String preference_default_element_names_start_with_uppercase_label;

    /*
     * -------------------------------------------------------------------
     * LegStar annotations default options
     */
    /** Annotations default presence. */
    public static String preference_default_add_legstar_annotations_label;

    /*
     * -------------------------------------------------------------------
     * COBOL compiler related options
     */
    /** Currency sign CURRENCY SIGN clause in the SPECIAL-NAMES) default. */
    public static String preference_default_currency_sign_label;

    /**
     * Currency symbol (CURRENCY PICTURE SYMBOL clause in the SPECIAL-NAMES)
     * default.
     */
    public static String preference_default_currency_symbol_label;

    /** Decimal point is comma default. */
    public static String preference_default_decimal_point_is_comma_label;

    /** National symbol DBCS default. */
    public static String preference_default_nsymbol_dbcs_label;

    /** Quote is quote default. */
    public static String preference_default_quote_is_quote_label;

    /** Ant generation background task label. */
    public static String ant_generating_task_label;
    /** Ant generation failure message. */
    public static String ant_failure_console_msg;
    /** Opening XML Schema editor background task label. */
    public static String editor_opening_task_label;

    /** Cobol to Xsd wizard title. */
    public static String cobol_To_xsd_wizard_page_title;
    /** Cobol to Xsd wizard description. */
    public static String cobol_To_xsd_wizard_page_description;
    /** No cobol fragment selected message. */
    public static String no_cobol_fragment_selected_msg;
    /** Hyperlink text tos select cobol fragments from file system. */
    public static String select_cobol_fragments_fs_label;
    /** Dialog title for COBOL file selection. */
    public static String select_cobol_fragments_dialog_title;

    /** Java to Xsd wizard title. */
    public static String java_To_xsd_wizard_page_title;
    /** Java to Xsd wizard description. */
    public static String java_To_xsd_wizard_page_description;
    /** No java classes are selected message. */
    public static String no_java_classes_selected_msg;
    /** Remove button label. */
    public static String remove_button_label;
    /** Selection dialog title. */
    public static String selection_dialog_title;
    /** Selection error dialog title. */
    public static String selection_error_dialog_title;
    /** Selecting java classes failed message (short). */
    public static String selection_dialog_init_failure_short_msg;
    /** Selecting java classes failed message (long). */
    public static String selection_dialog_init_failure_long_msg;
    /** Classpath initialization error dialog title. */
    public static String classpath_init_error_dialog_title;
    /** Retrieving classpath for java classes failed message (short). */
    public static String classpath_init_failure_short_msg;
    /** Retrieving classpath for java classes failed message (long). */
    public static String classpath_init_failure_long_msg;
    /** Generation error dialog title. */
    public static String generation_error_dialog_title;
    /** Generation failed message (short). */
    public static String generation_dialog_failure_short_msg;
    /** Generation failed message (long). */
    public static String generation_dialog_failure_long_msg;

    /** Main wizard title. */
    public static String main_wizard_page_title;
    /** Main wizard description. */
    public static String main_wizard_page_description;
    /** Source type label. */
    public static String source_type_group_label;
    /** Cobol source type. */
    public static String cobol_source_type_text;
    /** Xsd or WSDL source type. */
    public static String xsd_source_type_text;
    /** Java source type. */
    public static String java_source_type_text;
    /** Target group label. */
    public static String target_group_label;
    /** Container label. */
    public static String container_label;
    /** Container selection label. */
    public static String container_selection_label;
    /** XSD name label. */
    public static String xsd_file_name_label;
    /** Overwrite button label. */
    public static String overwrite_button_label;
    /** Namespace button label. */
    public static String namespace_label;
    /** No target container message. */
    public static String no_target_container_msg;
    /** Invalid target container message. */
    public static String invalid_target_container_msg;
    /** Invalid target xsd file message. */
    public static String no_target_xsd_file_name_msg;
    /** Xsd file already exists message. */
    public static String already_exists_target_xsd_file_msg;
    /** Invalid target namespace message. */
    public static String no_target_namespace_msg;

    /** Xsd to Xsd wizard title. */
    public static String xsd_To_xsd_wizard_page_title;
    /** Xsd to Xsd wizard description. */
    public static String xsd_To_xsd_wizard_page_description;
    /** No xsd source message. */
    public static String no_xsd_or_wsdl_source_msg;
    /** Type of url to lookup and keep in history. */
    public static String url_type_label;
    /** Switch to different namespace button. */
    public static String switch_namespace_button_label;
    /** Switch to different namespace button (alternative). */
    public static String switch_namespace_to_button_label;
    /** XML load error dialog title. */
    public static String xml_load_error_dialog_title;
    /** XML load failed message (short). */
    public static String xml_load_failure_short_msg;
    /** XML load failed message (long). */
    public static String xml_load_failure_long_msg;

    /** Menu action label. */
    public static String legstar_menu_label;

    static {
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

}
