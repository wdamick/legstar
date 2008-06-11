package com.legstar.eclipse.plugin.cixscom.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.legstar.eclipse.plugin.cixscom.Activator;

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

        store.setDefault(PreferenceConstants.CIXS_PACKAGE_NAME_PREFIX,
                "com.legstar.test.cixs");

        store.setDefault(PreferenceConstants.CIXS_TARGET_ANT_FOLDER, "ant");
        store.setDefault(PreferenceConstants.CIXS_TARGET_PROP_FOLDER, "prop");
    }

}
