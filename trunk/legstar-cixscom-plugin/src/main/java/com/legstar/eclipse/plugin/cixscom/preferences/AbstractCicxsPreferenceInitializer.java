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
package com.legstar.eclipse.plugin.cixscom.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Class used to initialize default preference values.
 */
public abstract class AbstractCicxsPreferenceInitializer extends AbstractPreferenceInitializer {

    /**
     * Preference initializers inheriting from this class must call this method
     * in order to initialize common fields.
     * @param store the data store where initial values are kept
     */
    public void initializeDefaultPreferences(final IPreferenceStore store) {

        store.setDefault(PreferenceConstants.DEFAULT_CIXS_PACKAGE_NAME_PREFIX,
                "com.legstar.test.cixs");
        store.setDefault(PreferenceConstants.DEFAULT_CIXS_TARGET_DIST_FOLDER, "dist");
    }

}
