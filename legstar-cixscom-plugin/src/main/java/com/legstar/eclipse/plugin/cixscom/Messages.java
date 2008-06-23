package com.legstar.eclipse.plugin.cixscom;

import org.eclipse.osgi.util.NLS;

/**
 * This plugin localized message class.
 */
public class Messages extends NLS {

	/** All messages come from this property file.*/
	private static final String BUNDLE_NAME =
		"com.legstar.eclipse.plugin.cixscom.messages";

	/** Preference page title.*/
	public static String cixscom_preference_page_title;
	/** No mapping file was specified.*/
	public static String no_mapping_file_msg;
	/** Description on preference page.*/
	public static String preference_page_description;
	/** Generated java classes package name prefix.*/
	public static String cixs_package_name_prefix;
	/** Target properties files folder.*/
	public static String cixs_properties_folder;
	
	/** Generation error dialog boxes title.*/
	public static String generate_error_dialog_title;
	/** Generation failure short message.*/
	public static String generation_failure_short_msg;
	/** Generation failure long message.*/
	public static String generation_failure_long_msg;
	
	/** Generation project group label.*/
	public static String generation_project_label;
	/** Generation project name label.*/
	public static String generation_project_name_label;
	/** Generation java package label.*/
	public static String generation_java_package_label;
	
	/** Group label for locations of binding classes.*/
	public static String structures_binding_classes_label;
	/** JAXB classes location label.*/
	public static String jaxb_classes_location_label;
	/** COXB classes location label.*/
	public static String coxb_classes_location_label;
	/** Custom classes location label.*/
	public static String cust_classes_location_label;
	
	/** Target locations label.*/
	public static String generation_target_locations;
	/** Java sources target location label.*/
	public static String java_sources_target_location;
	/** Java classes target location label.*/
	public static String java_classes_target_location;
	/** Ant scripts target location label.*/
	public static String ant_scripts_target_location;
	/** Properties files target location label.*/
	public static String properties_files_target_location;
	
	/** Deployment group label.*/
	public static String deployment_group_label;
	/** Mainframe character set label.*/
	public static String mainframe_charset_label;
	
	/** Invalid (empty) project name.*/
	public static String invalid_project_name_msg;
	/** Generic invalid location message.*/
	public static String invalid_location_msg;
	/** Invalid mainframe character set message.*/
	public static String invalid_mainframe_charset_msg;
	
	/** Lookup default java locations error message.*/
	public static String java_location_lookup_failure_msg;
	/** Invalid Java project error message.*/
	public static String invalid_java_project_msg;
	/** Invalid deafult location error message.*/
	public static String invalid_default_location_msg;
	/** Location refresh error message.*/
	public static String location_refresh_failure_msg;
	/** File load short error message.*/
	public static String mapping_file_load_failure_short_msg;
	/** File load long error message.*/
	public static String mapping_file_load_failure_long_msg;
	/** Generating ant script task label.*/
	public static String ant_generation_task_label;

	static {
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

}
