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
package com.legstar.eclipse.plugin.common;

import org.eclipse.osgi.util.NLS;

/**
 * This plugin localized message class.
 */
public class Messages extends NLS {

	/** All messages come from this property file.*/
	private static final String BUNDLE_NAME =
		"com.legstar.eclipse.plugin.common.messages";

	/** Preference page title.*/
	public static String common_preference_page_title;
	/** Menu action label.*/
	public static String action_label;
	
	/** Product logo icon.*/
	public static String legstar_logo_icon;
	/** Preference page description.*/
	public static String preference_page_description;
	/** Location of generation ant scripts (relative to Eclipse project).*/
	public static String ant_scripts_folder_label;
	/** Mainframe character set.*/
	public static String mainframe_charset_label;
	
	/** Browse button label.*/
	public static String browse_button_label;
	/** Error opening a file.*/
	public static String file_open_error_msg;
	/** URL hyperlink selection from file system .*/
	public static String url_select_from_file_system_text;
	/** URL file selection label.*/
	public static String url_select_a_file_label;
	/** Go button label.*/
	public static String go_button_label;
	
	/** Ant generation starting task label.*/
	public static String ant_generating_task_label;
	/** Ant run starting task label.*/
	public static String ant_running_task_label;
	/** Ant run returned an error stream.*/
	public static String ant_failure_stream_msg;
	/** Ant run returned an error code.*/
	public static String ant_failure_retcode_msg;
	/** Ant run returned an an error on console.*/
	public static String ant_failure_console_msg;

	static {
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

}
