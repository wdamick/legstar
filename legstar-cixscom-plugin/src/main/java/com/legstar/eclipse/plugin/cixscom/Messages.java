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
package com.legstar.eclipse.plugin.cixscom;

import org.eclipse.osgi.util.NLS;

/**
 * This plugin localized message class.
 */
public class Messages extends NLS {

    /** All messages come from this property file. */
    private static final String BUNDLE_NAME =
            "com.legstar.eclipse.plugin.cixscom.messages";

    /** Preference page title. */
    public static String cixscom_preference_page_title;
    /** No mapping file was specified. */
    public static String no_mapping_file_msg;
    /** Description on preference page. */
    public static String preference_page_description;
    /** Generated java classes package name prefix. */
    public static String cixs_package_name_prefix_label;
    /** Target distribution archives folder. */
    public static String cixs_distribution_folder_label;

    /** Generation error dialog boxes title. */
    public static String generate_error_dialog_title;
    /** Generation failure short message. */
    public static String generation_failure_short_msg;
    /** Generation failure long message. */
    public static String generation_failure_long_msg;

    /** Generation project group label. */
    public static String generation_project_label;
    /** Generation project name label. */
    public static String generation_project_name_label;
    /** Generation java package label. */
    public static String generation_java_package_label;

    /** Group label for locations of binding classes. */
    public static String structures_binding_classes_label;
    /** JAXB classes location label. */
    public static String jaxb_classes_location_label;
    /** COXB classes location label. */
    public static String coxb_classes_location_label;
    /** Custom classes location label. */
    public static String cust_classes_location_label;

    /** Target locations label. */
    public static String generation_target_locations;
    /** Java sources target location label. */
    public static String java_sources_target_location;
    /** Java classes target location label. */
    public static String java_classes_target_location;
    /** Ant scripts target location label. */
    public static String ant_scripts_target_location;
    /** Distribution files target location label. */
    public static String distribution_files_target_location;

    /** Deployment group label. */
    public static String deployment_group_label;
    /** Mainframe character set label. */
    public static String mainframe_charset_label;

    /** Invalid (empty) project name. */
    public static String invalid_project_name_msg;
    /** Generic invalid location message. */
    public static String invalid_location_msg;
    /** Invalid mainframe character set message. */
    public static String invalid_mainframe_charset_msg;

    /** Lookup default java locations error message. */
    public static String java_location_lookup_failure_msg;
    /** Invalid Java project error message. */
    public static String invalid_java_project_msg;
    /** Invalid deafult location error message. */
    public static String invalid_default_location_msg;
    /** Location refresh error message. */
    public static String location_refresh_failure_msg;
    /** File load short error message. */
    public static String mapping_file_load_failure_short_msg;
    /** File load long error message. */
    public static String mapping_file_load_failure_long_msg;
    /** Generating ant script task label. */
    public static String ant_generation_task_label;

    /** Target pojo group label. */
    public static String target_pojo_group_label;
    /** Target pojo class name label. */
    public static String target_pojo_class_name_label;
    /** Class selection dialog title. */
    public static String class_selection_error_dialog_title;
    /** Class selection short error message. */
    public static String class_selection_error_short_msg;
    /** Class selection long error message. */
    public static String class_selection_error_long_msg;
    /** Target pojo method name label. */
    public static String target_pojo_method_name_label;
    /** Invalid pojo class name message. */
    public static String invalid_target_pojo_class_name_msg;
    /** Invalid pojo method name message. */
    public static String invalid_target_pojo_method_name_msg;

    /** Target Web service group label. */
    public static String target_web_service_group_label;
    /** Target Web service Wsdl URL label. */
    public static String target_web_service_wsdl_url_label;
    /** Target Web service Wsdl service name label. */
    public static String target_web_service_wsdl_service_name_label;
    /** Target Web service Wsdl port name label. */
    public static String target_web_service_wsdl_port_name_label;
    /** Target Web service Wsdl target namespace label. */
    public static String target_web_service_wsdl_target_namespace_label;
    /** Invalid wsdl url message. */
    public static String invalid_wsdl_url_msg;
    /** Invalid wsdl service name message. */
    public static String invalid_wsdl_service_name_msg;
    /** Invalid wsdl port name message. */
    public static String invalid_wsdl_port_name_msg;
    /** Invalid wsdl namespace message. */
    public static String invalid_target_namespace_msg;

    /** Port selection dialog title. */
    public static String wsdl_port_selection_dialog_title;
    /** Port selection error dialog title. */
    public static String wsdl_port_selection_error_dialog_title;
    /** Wsdl access error message. */
    public static String wsdl_access_error_msg;
    /** Target namespace label. */
    public static String target_namespace_label;
    /** Services list label. */
    public static String services_list_label;
    /** Ports list label. */
    public static String ports_list_label;

    /** Settings for HTTP group label. */
    public static String http_transport_group_label;
    /** Host address on which HTTP server listens to mainframe clients label. */
    public static String http_host_label;
    /** Port on which HTTP listens to mainframe clients label. */
    public static String http_port_label;
    /** Path on which HTTP listens to mainframe clients label. */
    public static String http_path_label;
    /** User id for basic authentication. */
    public static String http_userid_label;
    /** Password for basic authentication. */
    public static String http_password_label;
    /** Choice of sample Cobol Http client label. */
    public static String sample_cobol_http_client_type_label;

    /** Invalid port number. */
    public static String invalid_http_port_number_msg;
    /** Invalid host. */
    public static String invalid_http_host_msg;
    /** Invalid path. */
    public static String invalid_http_path_msg;

    /** Project preference store access failure short message. */
    public static String project_preferences_access_failure_short_msg;
    /** Project preference store access failure long message. */
    public static String project_preferences_access_failure_long_msg;
    /** Plugin location failure short message. */
    public static String plugin_install_locate_failure_short_msg;
    /** Plugin location failure long message. */
    public static String plugin_install_locate_failure_long_msg;

    static {
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

}
