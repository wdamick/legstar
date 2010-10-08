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
package com.legstar.eclipse.plugin.schemagen.preferences;

/**
 * Constant definitions for plug-in preferences.
 */
public final class PreferenceConstants {

    /** Used to prevent conflicts with other plugins. */
    private static final String PREFERENCE_PREFIX = "com.legstar.eclipse.plugin.schemagen.";

    /*
     * -------------------------------------------------------------------
     * COBOL source format related options
     */
    /** The default code formatting. */
    public static final String DEFAULT_CODE_FORMAT =
            PREFERENCE_PREFIX + "defaultCodeFormat";

    /** The default starting column. */
    public static final String DEFAULT_START_COLUMN =
            PREFERENCE_PREFIX + "defaultStartColumn";

    /** The default ending column. */
    public static final String DEFAULT_END_COLUMN =
            PREFERENCE_PREFIX + "defaultEndColumn";
    /*
     * -------------------------------------------------------------------
     * XML Schema related default options
     */
    /** A prefix to be used for new XML namespaces. */
    public static final String XSD_NAMESPACE_PREFIX =
            PREFERENCE_PREFIX + "xsdNamespacePrefix";

    /** XML schema default encoding. */
    public static final String DEFAULT_XSD_ENCODING =
            PREFERENCE_PREFIX + "defaultXsdEncoding";

    /** XML schema default mapping of 88 conditions to facets. */
    public static final String DEFAULT_XSD_MAP_CONDITIONS_TO_FACETS =
            PREFERENCE_PREFIX + "defaultMapConditionsToFacets";

    /** XML schema default customization XSLT file name. */
    public static final String DEFAULT_XSD_CUSTOM_XSLT_FILE_NAME =
            PREFERENCE_PREFIX + "defaultCustomXsltFileName";

    /** XML schema default prepending parent name if name conflict. */
    public static final String DEFAULT_XSD_NAME_CONFLICT_PREPEND_PARENT_NAME =
            PREFERENCE_PREFIX + "defaultNameConflictPrependParentName";

    /** XML schema default element name starting with uppercase. */
    public static final String DEFAULT_XSD_ELEMENT_NAMES_START_WITH_UPPERCASE =
            PREFERENCE_PREFIX + "defaultElementNamesStartWithUppercase";

    /*
     * -------------------------------------------------------------------
     * LegStar annotations default options
     */
    /** Annotations default presence. */
    public static final String DEFAULT_ADD_LEGSTAR_ANNOTATIONS =
            PREFERENCE_PREFIX + "defaultAddLegStarAnnotations";

    /*
     * -------------------------------------------------------------------
     * COBOL compiler related options
     */
    /** Currency sign (CURRENCY SIGN clause in the SPECIAL-NAMES) default. */
    public static final String DEFAULT_CURRENCY_SIGN =
            PREFERENCE_PREFIX + "defaultCurrencySign";

    /**
     * Currency symbol (CURRENCY PICTURE SYMBOL clause in the SPECIAL-NAMES)
     * default.
     */
    public static final String DEFAULT_CURRENCY_SYMBOL =
            PREFERENCE_PREFIX + "defaultCurrencySymbol";

    /** Decimal point is comma default. */
    public static final String DEFAULT_DECIMAL_POINT_IS_COMMA =
            PREFERENCE_PREFIX + "defaultDecimalPointIsComma";

    /** National symbol DBCS default. */
    public static final String DEFAULT_NSYMBOL_DBCS =
            PREFERENCE_PREFIX + "defaultNSymbolDbcs";

    /** Quote is quote default. */
    public static final String DEFAULT_QUOTE_IS_QUOTE =
            PREFERENCE_PREFIX + "defaultQuoteIsQuote";

    /** Last eclipse container used. */
    public static final String LAST_TARGET_CONTAINER =
            PREFERENCE_PREFIX + "last.targetcontainer";

    /** Last target XSD file name used. */
    public static final String LAST_TARGET_XSD_FILE_NAME =
            PREFERENCE_PREFIX + "last.targetxsdfilename";

    /** Utility class. */
    private PreferenceConstants() {

    }

}
