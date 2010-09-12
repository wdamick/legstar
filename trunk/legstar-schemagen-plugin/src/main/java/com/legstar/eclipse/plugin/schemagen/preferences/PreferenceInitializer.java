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
package com.legstar.eclipse.plugin.schemagen.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.legstar.cob2xsd.Cob2XsdContext;
import com.legstar.cob2xsd.Cob2XsdContext.CodeFormat;
import com.legstar.eclipse.plugin.schemagen.Activator;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.core.runtime.preferences.
     *      AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    public void initializeDefaultPreferences() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();

        /*
         * -------------------------------------------------------------------
         * COBOL source format related options
         */
        store.setDefault(PreferenceConstants.DEFAULT_CODE_FORMAT,
                CodeFormat.FIXED_FORMAT.toString());

        store.setDefault(PreferenceConstants.DEFAULT_START_COLUMN,
                Cob2XsdContext.DEFAULT_START_COLUMN);

        store.setDefault(PreferenceConstants.DEFAULT_END_COLUMN,
                Cob2XsdContext.DEFAULT_END_COLUMN);

        /*
         * -------------------------------------------------------------------
         * XML Schema related default options
         */
        store.setDefault(PreferenceConstants.XSD_NAMESPACE_PREFIX,
                "http://coxb.test.legstar.com");
        store.setDefault(PreferenceConstants.DEFAULT_XSD_ENCODING,
                Cob2XsdContext.DEFAULT_XSD_ENCODING);
        store.setDefault(
                PreferenceConstants.DEFAULT_XSD_MAP_CONDITIONS_TO_FACETS,
                        false);
        store.setDefault(PreferenceConstants.DEFAULT_XSD_CUSTOM_XSLT_FILE_NAME,
                "");
        store
                .setDefault(
                        PreferenceConstants.DEFAULT_XSD_NAME_CONFLICT_PREPEND_PARENT_NAME,
                        false);
        store
                .setDefault(
                        PreferenceConstants.DEFAULT_XSD_ELEMENT_NAMES_START_WITH_UPPERCASE,
                        false);

        /*
         * -------------------------------------------------------------------
         * LegStar annotations default options
         */
        store.setDefault(PreferenceConstants.DEFAULT_ADD_LEGSTAR_ANNOTATIONS,
                true);

        /*
         * -------------------------------------------------------------------
         * COBOL compiler related options
         */
        store.setDefault(PreferenceConstants.DEFAULT_CURRENCY_SIGN,
                Cob2XsdContext.DEFAULT_CURRENCY_SIGN);
        store.setDefault(PreferenceConstants.DEFAULT_CURRENCY_SYMBOL,
                Cob2XsdContext.DEFAULT_CURRENCY_SYMBOL);
        store.setDefault(PreferenceConstants.DEFAULT_DECIMAL_POINT_IS_COMMA,
                false);
        store.setDefault(PreferenceConstants.DEFAULT_NSYMBOL_DBCS, false);
        store.setDefault(PreferenceConstants.DEFAULT_QUOTE_IS_QUOTE, true);
    }

}
