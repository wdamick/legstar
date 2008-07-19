package com.legstar.eclipse.plugin.mulegen.preferences;

/**
 * Constant definitions for plug-in preferences.
 */
public class PreferenceConstants {

    /** Where Mule is installed. */
    public static final String MULE_INSTALL_FOLDER =
        "muleInstallFolder";
    
    /** Mule folder where user jar files should be deployed. */
    public static final String MULE_USER_JAR_FOLDER =
        "com.legstar.eclipse.plugin.mulegen.UserJarFolder";

    /** Folder where COBOL Client code should be generated. */
    public static final String COBOL_SAMPLE_FOLDER =
        "com.legstar.eclipse.plugin.mulegen.CixsCobolFolder";

    /** Service URI exposed by Mule to Mainframe clients. */
    public static final String SERVICE_URI =
        "com.legstar.eclipse.plugin.mulegen.CixsServiceURI";

    /** Mainframe URI exposed to Mule clients. */
    public static final String HOST_URI =
        "com.legstar.eclipse.plugin.mulegen.CixsHostURI";

    /** Folder where configuration files should be generated. */
    public static final String TARGET_MULE_CONFIG_FOLDER =
        "com.legstar.eclipse.plugin.mulegen.CixsConfigFolder";
}
