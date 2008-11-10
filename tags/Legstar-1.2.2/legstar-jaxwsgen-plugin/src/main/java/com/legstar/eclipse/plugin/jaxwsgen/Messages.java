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
package com.legstar.eclipse.plugin.jaxwsgen;

import org.eclipse.osgi.util.NLS;

/**
 * This plugin localized message class.
 */
public class Messages extends NLS {

	/** All messages come from this property file.*/
	private static final String BUNDLE_NAME =
		"com.legstar.eclipse.plugin.jaxwsgen.messages";

	/** The jaxws to cixs wizard icon.*/
	public static String jaxws_to_cixs_generator_icon;
	/** The cixs to jaxws wizard icon.*/
	public static String cixs_to_jaxws_generator_icon;
	/** The name filter to which this wizard is associated.*/
	public static String filename_filter;
	/** Right click on cixs files shows this menu item.*/
	public static String popup_menu_label;
	/** Jaxws to cixs action menu label.*/
	public static String jaxws_to_cixs_action_label;
	/** Cixs to jaxws action menu label.*/
	public static String cixs_to_jaxws_action_label;
	
	/** Preference page title.*/
	public static String jaxwsgen_preference_page_title;
	
	/** Port selection dialog title.*/
	public static String wsdl_port_selection_dialog_title;
	/** Port selection error dialog title.*/
	public static String wsdl_port_selection_error_dialog_title;
	/** Wsdl access error message.*/
	public static String wsdl_access_error_msg;
	/** Target namespace label.*/
	public static String target_namespace_label;
	/** Services list label.*/
	public static String services_list_label;
	/** Ports list label.*/
	public static String ports_list_label;
	
	/** Preference page description.*/
	public static String preference_page_description;
	/** War deployment preference label.*/
	public static String preference_war_location_label;
	/** Web descriptor folder preference label.*/
	public static String preference_wdd_folder_label;
	/** Target namespace preference label.*/
	public static String preference_ws_namespace_label;
	/** Cobol samples folder preference label.*/
	public static String preference_cobol_folder_label;
	/** Proxy URI template preference label.*/
	public static String preference_proxy_uri_template_label;
	
	/** Cixs to jaxws generator name.*/
	public static String cixs_to_jaxws_wizard_page_title;
	/** Cixs to jaxws generator description.*/
	public static String cixs_to_jaxws_wizard_page_description;
	
	/** Wsdl group label.*/
	public static String wsdl_group_label;
	/** Wsdl URL label.*/
	public static String wsdl_url_label;
	/** Wsdl service name label.*/
	public static String wsdl_service_name_label;
	/** Wsdl port name label.*/
	public static String wsdl_port_name_label;
	/** Wsdl target namespace label.*/
	public static String wsdl_target_namespace_label;
	
	/** Target location for web descriptors label.*/
	public static String wdd_target_location_label;
	/** Target location for COBOL samples label.*/
	public static String cobol_target_location_label;
	/** URI exposed to mainframe label.*/
	public static String proxy_uri_label;
	/** User ID to access proxy label.*/
	public static String proxy_user_id_label;
	/** Password to access proxy label.*/
	public static String proxy_password_label;
	/** War deployment location label.*/
	public static String war_deployment_location_label;
	
	/** Invalid wsdl url message.*/
	public static String invalid_wsdl_url_msg;
	/** Invalid wsdl service name message.*/
	public static String invalid_wsdl_service_name_msg;
	/** Invalid wsdl port name message.*/
	public static String invalid_wsdl_port_name_msg;
	/** Invalid web descriptors location message.*/
	public static String invalid_wdd_target_location_msg;
	/** Invalid cobol samples location message.*/
	public static String invalid_cobol_target_location_msg;
	/** Invalid wsdl namespace message.*/
	public static String invalid_target_namespace_msg;

	
	/** Jaxws to cixs generator name.*/
	public static String jaxws_to_cixs_wizard_page_title;
	/** Jaxws to cixs generator description.*/
	public static String jaxws_to_cixs_wizard_page_description;
	/** Adapter  Web Service target namespace.*/
	public static String adapter_target_namespace_label;
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

}
