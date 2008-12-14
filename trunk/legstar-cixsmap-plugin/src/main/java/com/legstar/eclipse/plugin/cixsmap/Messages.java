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
package com.legstar.eclipse.plugin.cixsmap;

import org.eclipse.osgi.util.NLS;

/**
 * This plugin localized message class.
 */
public class Messages extends NLS {

    /** All messages come from this property file.*/
    private static final String BUNDLE_NAME =
        "com.legstar.eclipse.plugin.cixsmap.messages";

    /** The wizard icon.*/
    public static String operations_mapping_icon;
    /** The operations mapping file suffix.*/
    public static String operations_mapping_file_suffix;
    /** New mapping file name. */
    public static String new_operations_mapping_file_name_label;

    /** Mapping Editor name. */
    public static String editor_title;
    /** Mapping file name is invalid message. */
    public static String invalid_mapping_file_msg;
    /** Mapping file name extension is invalid message. */
    public static String invalid_mapping_file_extension_msg;
    /** Toolbar button tooltip.. */
    public static String action_tooltip;
    /** Menu option label. */
    public static String action_label;
    /** New file action name. */
    public static String new_file_title;

    /** New file wizard title.*/
    public static String new_file_wizard_page_title;
    /** New file wizard description.*/
    public static String new_file_wizard_description;

    /** Dialog title. */
    public static String generate_dialog_title;
    /** Error dialog box title. */
    public static String generate_error_dialog_title;
    /** No generators registered. */
    public static String no_generators_found_msg;
    /** Looking up generators failed. */
    public static String listing_generators_failed_msg;
    /** Starting a generators wizard failed. */
    public static String launching_generator_wizard_failed_msg;

    /** Dialog box title. */
    public static String operation_mapping_dialog_title;
    /** Error dialog box title. */
    public static String operation_mapping_error_dialog_title;
    /** Operation name label. */
    public static String operation_name_label;
    /** Operation program label. */
    public static String operation_program_label;
    /** Operation channel label. */
    public static String operation_channel_label;
    /** Operation input label. */
    public static String operation_input_structures_label;
    /** Operation output label. */
    public static String operation_output_structures_label;
    /** Mapping name label. */
    public static String operations_mapping_name_label;
    /** Missing opration name message. */
    public static String no_operation_name_msg;
    /** Missing program name message. */
    public static String no_program_msg;
    /** Program name is too long. */
    public static String program_name_too_long_msg;
    /** Missing channel message. */
    public static String container_without_channel_msg;
    /** Missing container message. */
    public static String channel_without_container_msg;


    /** Add button label. */
    public static String add_button_label;
    /** Modify button label. */
    public static String edit_button_label;
    /** Delete button label. */
    public static String delete_button_label;
    /** Generate button label. */
    public static String generate_button_label;
    /** Number of input structures column label. */
    public static String operation_input_structures_nbr_label;
    /** Number of output structures column label. */
    public static String operation_output_structures_nbr_label;
    /** No operations selected error message. */
    public static String no_operations_selected_msg;
    /** Confirm operation remove message. */
    public static String confirm_operation_delete_msg;
    /** No operations defined error message. */
    public static String no_operations_msg;

    /** Dialog box title. */
    public static String structure_mapping_dialog_title;
    /** Dialog box title. */
    public static String structure_mapping_error_dialog_title;
    /** COBOL root data item name label. */
    public static String structure_cobol_root_label;
    /** CICS container name label. */
    public static String structure_container_label;
    /** Package name of jaxb type. */
    public static String structure_jaxb_package_label;
    /** Name of JAXB class. */
    public static String structure_jaxb_type_label;
    /** No structures selected error message. */
    public static String no_structure_selected_msg;
    /** Confirm structure remove message. */
    public static String confirm_structure_delete_msg;

    /** Not a valid java project. */
    public static String invalid_java_project_msg;
    /** Something wrong with installation. */
    public static String setup_error_msg;
    /** No binding classes found in project.*/
    public static String no_coxb_classes_in_project_msg;
    /** No binding classes found in package.*/
    public static String no_coxb_classes_in_package_msg;
    /** No JAXB type selected.*/
    public static String no_jaxb_type_selected_msg;
    /** COBOL name is invalid.*/
    public static String invalid_structure_cobol_name_msg;
    /** COBOL name resolver failure.*/
    public static String cobol_name_validation_failure_msg;

    /** All editors messages are formatted like so. */
    public static String editor_generic_error_msg;
    /** Editor loading model failure. */
    public static String editor_error_loading_msg;
    /** Editor saving model failure. */
    public static String editor_error_saving_msg;
    /** Editor text label. */
    public static String editor_text_label;
    /** Editor error ctreating nested text. */
    public static String editor_error_creating_nested_text_msg;
    /** Editor creating file task. */
    public static String editor_creating_file_task_label;
    /** Editor opening file task. */
    public static String editor_opening_file_task_label;

    /** Menu action label.*/
    public static String legstar_menu_label;

    static {
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

}
