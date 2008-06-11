package com.legstar.eclipse.plugin.jaxwsgen.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.legstar.eclipse.plugin.jaxwsgen.Activator;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

    /**
     * {@inheritDoc}
     * @see org.eclipse.core.runtime.preferences.
     * AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    public void initializeDefaultPreferences() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();

        store.setDefault(PreferenceConstants.J2EE_INSTALL_FOLDER,
                getDefaultJ2eeLocation());

        store.setDefault(PreferenceConstants.J2EE_WAR_FOLDER,
                getDefaultJ2eeLocation() + "/webapps");

        store.setDefault(PreferenceConstants.J2EE_WDD_FOLDER,
        		"WebContent/WEB-INF");

        store.setDefault(PreferenceConstants.WS_TARGET_NAMESPACE_PREFIX,
				"http://test.legstar");
    }

    /**
     * If Tomcat or Jetty are installed on this machine, this will retrieve the
     * installation folder.
     * @return the default location
     */
    public String getDefaultJ2eeLocation() {
        String value = System.getenv("CATALINA_BASE");
        if (value == null) {
        	value = System.getenv("CATALINA_HOME");
        	if (value == null) {
            	value = System.getenv("JETTY_HOME");
                if (value == null) {
                    return "";
                }
        	}
        }
        return value;
    }

}
