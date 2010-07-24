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

/**
 * Constant definitions for plug-in preferences.
 */
public final class PreferenceConstants {

 
    /* -------------------------------------------------------------------
     * XML Schema related default options
     * */
    /** A prefix to be used for new XML namespaces. */
    public static final String XSD_NAMESPACE_PREFIX =
        "com.legstar.eclipse.plugin.schemagen.xsdNamespacePrefix";

    /** XML schema default encoding. */
    public static final String DEFAULT_XSD_ENCODING =
        "com.legstar.eclipse.plugin.schemagen.defaultXsdEncoding";

    /** XML schema default mapping of 88 conditions to facets. */
    public static final String DEFAULT_XSD_MAP_CONDITIONS_TO_FACETS =
        "com.legstar.eclipse.plugin.schemagen.defaultMapConditionsToFacets";

    /** XML schema default customization XSLT file name. */
    public static final String DEFAULT_XSD_CUSTOM_XSLT_FILE_NAME =
        "com.legstar.eclipse.plugin.schemagen.defaultCustomXsltFileName";

    /** XML schema default prepending parent name if name conflict. */
    public static final String DEFAULT_XSD_NAME_CONFLICT_PREPEND_PARENT_NAME =
        "com.legstar.eclipse.plugin.schemagen.defaultNameConflictPrependParentName";

    /** XML schema default element name starting with uppercase. */
    public static final String DEFAULT_XSD_ELEMENT_NAMES_START_WITH_UPPERCASE =
        "com.legstar.eclipse.plugin.schemagen.defaultElementNamesStartWithUppercase";
    
    /* -------------------------------------------------------------------
     * LegStar annotations default options
     * */
    /** Annotations default presence. */
    public static final String DEFAULT_ADD_LEGSTAR_ANNOTATIONS =
        "com.legstar.eclipse.plugin.schemagen.defaultAddLegStarAnnotations";
    
    /** Prefix to be used for new jaxb package names. */
    public static final String JAXB_PACKAGE_NAME_PREFIX =
        "com.legstar.eclipse.plugin.schemagen.jaxbPackageNamePrefix";

    /** Annotations default suffix for type names. */
    public static final String DEFAULT_JAXB_TYPE_CLASSES_SUFFIX =
        "com.legstar.eclipse.plugin.schemagen.defaultJaxbTypeClassesSuffix";
    
   
    /* -------------------------------------------------------------------
     * COBOL compiler related options
     * */
    /** Currency sign (CURRENCY SIGN clause in the SPECIAL-NAMES) default. */
    public static final String DEFAULT_CURRENCY_SIGN =
        "com.legstar.eclipse.plugin.schemagen.defaultCurrencySign";

    /** Currency symbol (CURRENCY PICTURE SYMBOL clause in the SPECIAL-NAMES) default. */
    public static final String DEFAULT_CURRENCY_SYMBOL =
        "com.legstar.eclipse.plugin.schemagen.defaultCurrencySymbol";

    /** Decimal point is comma default. */
    public static final String DEFAULT_DECIMAL_POINT_IS_COMMA =
        "com.legstar.eclipse.plugin.schemagen.defaultDecimalPointIsComma";

    /** National symbol DBCS default. */
    public static final String DEFAULT_NSYMBOL_DBCS =
        "com.legstar.eclipse.plugin.schemagen.defaultNSymbolDbcs";

    /** Quote is quote default. */
    public static final String DEFAULT_QUOTE_IS_QUOTE =
        "com.legstar.eclipse.plugin.schemagen.defaultQuoteIsQuote";


    
    
    /** Last eclipse container used. */
    public static final String LAST_TARGET_CONTAINER =
        "com.legstar.eclipse.plugin.schemagen.last.targetcontainer";

    /** Last target XSD file name used. */
    public static final String LAST_TARGET_XSD_FILE_NAME =
        "com.legstar.eclipse.plugin.schemagen.last.targetxsdfilename";

    /** Last JAXB type suffix used. */
    public static final String LAST_JAXB_TYPE_SUFFIX =
        "com.legstar.eclipse.plugin.schemagen.last.jaxbtypesuffix";

    /** Utility class.*/
    private PreferenceConstants() {
        
    }
    
}
