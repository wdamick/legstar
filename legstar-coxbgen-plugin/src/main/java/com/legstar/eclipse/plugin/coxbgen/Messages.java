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
package com.legstar.eclipse.plugin.coxbgen;

import org.eclipse.osgi.util.NLS;

/**
 * This plugin localized message class.
 */
public class Messages extends NLS {

    /** All messages come from this property file. */
    private static final String BUNDLE_NAME =
            "com.legstar.eclipse.plugin.coxbgen.messages";

    /** The wizards icon. */
    public static String binding_generator_icon;
    /** The name filter to which this wizard is associated. */
    public static String filename_filter;
    /** Right click on XML Schema files shows this menu item. */
    public static String popup_menu_label;
    /** Action menu label. */
    public static String action_label;

    /** Wizard page title. */
    public static String wizard_page_title;
    /** Wizard page description. */
    public static String wizard_page_description;

    /** No XSD file was selected. */
    public static String no_xsd_file_msg;
    /** Generation error dialog boxes title. */
    public static String generate_error_dialog_title;
    /** Generation error short message. */
    public static String generation_failure_short_msg;
    /** Generation error long message. */
    public static String generation_failure_long_msg;

    /** No root element selected error message. */
    public static String no_root_elements_selected_msg;
    /** Target source folder is not valid error message. */
    public static String invalid_target_src_folder_msg;
    /** Unable to setup classpath for the target java project. */
    public static String classpath_setup_failure_msg;

    /** JAXB group label. */
    public static String jaxb_group_label;
    /** XML Schema file name label. */
    public static String xsd_file_name_label;
    /** JAXB package name label. */
    public static String jaxb_package_name_label;
    /** JAXB options button label. */
    public static String jaxb_options_button_label;
    /** JAXB options dialog title. */
    public static String jaxb_options_dialog_title;
    /** JAXB xjb options group title. */
    public static String jaxb_xjb_options_group_title;

    /** JAXB/XJB generate isset methods label. */
    public static String jaxb_xjb_generate_issetmethod_label;
    /** JAXB/XJB serializable ID label. */
    public static String jaxb_xjb_serializableid_label;
    /** JAXB/XJB type name prefix label. */
    public static String jaxb_xjb_typenameprefix_label;
    /** JAXB/XJB type name suffix label. */
    public static String jaxb_xjb_typenamesuffix_label;
    /** JAXB/XJB element name prefix label. */
    public static String jaxb_xjb_elementnameprefix_label;
    /** JAXB/XJB element name suffix label. */
    public static String jaxb_xjb_elementnamesuffix_label;

    /** COXB package name label. */
    public static String coxb_package_name_label;
    /** COXB options button label. */
    public static String coxb_options_button_label;
    /** Available root element list label. */
    public static String root_elements_list_label;
    /** Target source folder label. */
    public static String target_source_folder_label;
    /** Target source folder selection dialog title. */
    public static String target_source_folder_select_label;
    /** Target classes folder label. */
    public static String target_classes_folder_label;

    /** COXB options dialog title. */
    public static String coxb_options_dialog_title;
    /** COXB generate XML Transformers option. */
    public static String coxb_options_xmltransformers_label;
    /** COXB generate JSON Transformers option. */
    public static String coxb_options_jsontransformers_label;
    /** COXB more transformers options group title. */
    public static String coxb_more_transformers_options_group_title;

    /** Was unable to initialize page. */
    public static String page_initialization_failure_msg;
    /** XML schema belongs to an invalid project. */
    public static String xsd_file_in_invalid_project_msg;
    /** Generating task label. */
    public static String ant_generating_task_label;

    static {
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

}
