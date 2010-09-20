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
package com.legstar.jaxb.plugin;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

import javax.xml.bind.annotation.XmlSchemaType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Element;
import org.xml.sax.ErrorHandler;

import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolMarkup;
import com.legstar.coxb.CobolType;
import com.sun.codemodel.JAnnotationUse;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JExpr;
import com.sun.codemodel.JFieldVar;
import com.sun.tools.xjc.Options;
import com.sun.tools.xjc.Plugin;
import com.sun.tools.xjc.model.CElement;
import com.sun.tools.xjc.model.CElementInfo;
import com.sun.tools.xjc.model.CPluginCustomization;
import com.sun.tools.xjc.model.CReferencePropertyInfo;
import com.sun.tools.xjc.outline.ClassOutline;
import com.sun.tools.xjc.outline.FieldOutline;
import com.sun.tools.xjc.outline.Outline;

/**
 * This is an extension to the JAXB XJC plugin. It is being invoked by the JAXB
 * XML to Java compilation and injects supplementary cobol annotations into the
 * generated Java classes.
 * Add -Dcom.sun.tools.xjc.Options.findServices=true to VM arguments to help
 * solve classpath issues.
 * 
 */
public class CobolJAXBAnnotator extends Plugin {

    /**
     * Constant values used throughout the annotator.
     */
    /** Option passed to XJC to enable this cobol plugin. */
    public static final String OPTION_NAME = "Xlegstar-code";

    /** Logger to be used only at development time (messes up ant output). */
    private final Log _log = LogFactory.getLog(getClass());

    /** Command line help for cobol plugin XJC option. */
    public static final String OPTION_USAGE =
            "  -Xlegstar-code      :  inject cobol binding annotation into the "
                    + "generated code";

    /** {@inheritDoc} */
    @Override
    /** This will let XJC know that this plugin supports this argument*/
    public String getOptionName() {
        return OPTION_NAME;
    }

    /** {@inheritDoc} */
    @Override
    /** Just in case XJC requires a friendly comment on this plugin */
    public String getUsage() {
        return OPTION_USAGE;
    }

    /** {@inheritDoc} */
    @Override
    /** This lets XJC know what are the namespaces to watch for in the source
     *  schema */
    public List < String > getCustomizationURIs() {
        return Collections.singletonList(CobolMarkup.NS);
    }

    /** {@inheritDoc} */
    @Override
    /** Just to be extra sure, XJC will call us on each element from the source
     * schema that seems to belong to the namespaces to watch for. We need to 
     * tell XJC whether this is an actual supported customization. */
    public boolean isCustomizationTagName(
            final String nsUri,
            final String localName) {

        return (nsUri.equals(CobolMarkup.NS) && (localName
                .equals(CobolMarkup.ELEMENT)
                        || localName.equals(CobolMarkup.ELEMENT_VALUE) || localName
                .equals(CobolMarkup.COMPLEX_TYPE)));
    }

    /** {@inheritDoc} */
    @Override
    /** 
     * This is where the real action takes place. XJC has done its job of
     * building an in-memory model of the soon-to-be generated java classes.
     * We are given a chance to change that model so that the generated
     * classes will include the extra annotations that we need.
     *  */
    public boolean run(
            final Outline model,
            final Options opt,
            final ErrorHandler errorHandler) {

        long start = System.currentTimeMillis();

        /*
         * Each simpleType at the root level in the source schema will become
         * a JAXBElement in the ObjectFactory class. .
         */
        for (CElementInfo eo : model.getModel().getAllElements()) {

            if (_log.isDebugEnabled()) {
                _log.debug("CobolJAXBAnnotator::run::CElementInfo::"
                        + eo.fullName());
            }
            CPluginCustomization c =
                    eo.getCustomizations().find(
                            CobolMarkup.NS, CobolMarkup.ELEMENT);
            if (c == null) {
                continue; // no customization --- nothing to inject here
            }

            /* Mark the annotation as acknowledged */
            c.markAsAcknowledged();

        }

        /*
         * Each complexType in the source schema will result in a class outline
         * and its own implementation class.
         */
        for (ClassOutline co : model.getClasses()) {

            if (_log.isDebugEnabled()) {
                _log.debug(
                        "CobolJAXBAnnotator::run::ClassOutline::"
                                + co.implClass);
            }
            annotateClass(co);

            for (FieldOutline fo : co.getDeclaredFields()) {

                if (_log.isDebugEnabled()) {
                    _log.debug(
                            "CobolJAXBAnnotator::run::FieldOutline::"
                                    + fo.getPropertyInfo().getName(false));
                }

                /*
                 * Get the customization depending on whether this is a direct
                 * element or a reference to an element.Elements such as arrays
                 * of hexBinary will result in a CReferencePropertyInfo
                 */
                CPluginCustomization c = null;
                if (fo.getPropertyInfo() instanceof CReferencePropertyInfo) {
                    if (_log.isDebugEnabled()) {
                        _log.debug(
                                "FieldOutline is CReferencePropertyInfo");
                    }

                    for (CElement ce : ((CReferencePropertyInfo) fo
                            .getPropertyInfo()).
                            getElements()) {
                        c = ce.getCustomizations().find(
                                CobolMarkup.NS, CobolMarkup.ELEMENT);
                    }
                } else {
                    c = fo.getPropertyInfo().getCustomizations().find(
                            CobolMarkup.NS, CobolMarkup.ELEMENT);
                }

                if (c == null) {
                    continue; // no customization --- nothing to inject here
                }
                if (_log.isDebugEnabled()) {
                    String javaType = fo.getRawType().name();
                    _log.debug(
                            "CobolJAXBAnnotator::run::ClassOutline::"
                                    + c.element.getLocalName()
                                    + " type=" + javaType);
                }

                c.markAsAcknowledged();

                /* Inject a cobol annotation on this field. */
                JFieldVar jf =
                        co.implClass.fields().get(
                                fo.getPropertyInfo().getName(false));
                JAnnotationUse ce = jf.annotate(CobolElement.class);

                mapAnnotations(c, ce);

                setDefaultValue(model.getCodeModel(), jf, c.element);

                /*
                 * HexBinary items are missing a JAXB annotation that
                 * we inject here
                 */
                if (fo.getRawType().name().compareTo("byte[]") == 0) {
                    JAnnotationUse xmlSchemaType =
                            jf.annotate(XmlSchemaType.class);
                    xmlSchemaType.param("name", "hexBinary");
                }

            }
        }

        long end = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Cobol annotation success.");
            _log.debug("Duration=" + (end - start) + " ms");
        }

        return true;
    }

    /**
     * Attempts to set a default value for the java field based on the
     * COBOL default value.
     * <p/>
     * Will not attempt to initialize arrays or complex types.
     * <p/>
     * Strings which COBOL peer defaults to low values or high values are
     * initialized with an empty string.
     * <p/>
     * Leading plus signs are removed from numerics, they cause
     * NumberFormatException.
     * 
     * @param codeModel the code model
     * @param jf the java field
     * @param e the XML node holding COBOL annotations
     */
    private void setDefaultValue(
            final JCodeModel codeModel, final JFieldVar jf, final Element e) {
        if (!e.hasAttribute(CobolMarkup.VALUE)) {
            return;
        }
        String value = e.getAttribute(CobolMarkup.VALUE).trim();
        String type = jf.type().binaryName();
        if (type.equals("java.lang.String")) {
            jf.init(JExpr.lit(value));
        } else {
            /* Assume a numeric from now on */
            if (value.length() == 0) {
                return;
            }
            /* Java does not like leading plus sign */
            if (value.startsWith("+")) {
                value = value.substring(1);
            }
            if (type.equals("java.math.BigDecimal")) {
                jf.init(JExpr.direct("new BigDecimal(\"" + value + "\")"));
            } else if (type.equals("java.math.BigInteger")) {
                jf.init(JExpr.direct("new BigInteger(\"" + value + "\")"));
            } else if (type.equals("short")) {
                jf.init(JExpr.lit(Short.parseShort(value)));
            } else if (type.equals("int")) {
                jf.init(JExpr.lit(Integer.parseInt(value)));
            } else if (type.equals("long")) {
                jf.init(JExpr.lit(Long.parseLong(value)));
            } else if (type.equals("float")) {
                jf.init(JExpr.lit(Float.parseFloat(value)));
            } else if (type.equals("double")) {
                jf.init(JExpr.lit(Double.parseDouble(value)));
            }
        }

    }

    /**
     * Propagate xsd complex type annotations on a class type.
     * 
     * @param co the class outline
     */
    private void annotateClass(final ClassOutline co) {
        CPluginCustomization c = co.target.getCustomizations().find(
                CobolMarkup.NS, CobolMarkup.COMPLEX_TYPE);
        if (c == null) {
            return; // no customization --- nothing to inject here
        }
        c.markAsAcknowledged();

        JAnnotationUse ce = co.implClass.annotate(CobolComplexType.class);
        ce.param(CobolMarkup.JAVA_CLASS_NAME,
                c.element.getAttribute(CobolMarkup.JAVA_CLASS_NAME));
    }

    /**
     * Each annotation is extracted from the XML schema customization and
     * injected back into the JAXB class code.
     * 
     * @param c the XML Schema annotation element
     * @param ce the Java code Cobol annotation
     */
    private void mapAnnotations(
            final CPluginCustomization c,
            final JAnnotationUse ce) {

        ce.param("cobolName",
                c.element.getAttribute(CobolMarkup.COBOL_NAME));

        String cobolType = c.element.getAttribute(CobolMarkup.TYPE);

        ce.param("type", CobolType.valueOf(cobolType));

        setNumericParm(c.element, CobolMarkup.LEVEL_NUMBER, ce);
        setBooleanParm(c.element, CobolMarkup.IS_JUSTIFIED_RIGHT, ce);
        setBooleanParm(c.element, CobolMarkup.IS_SIGNED, ce);
        setBooleanParm(c.element, CobolMarkup.IS_SIGN_LEADING, ce);
        setBooleanParm(c.element, CobolMarkup.IS_SIGN_SEPARATE, ce);
        setNumericParm(c.element, CobolMarkup.TOTAL_DIGITS, ce);
        setNumericParm(c.element, CobolMarkup.FRACTION_DIGITS, ce);
        setNumericParm(c.element, CobolMarkup.MIN_OCCURS, ce);
        setNumericParm(c.element, CobolMarkup.MAX_OCCURS, ce);
        setStringParm(c.element, CobolMarkup.DEPENDING_ON, ce);
        setBooleanParm(c.element, CobolMarkup.IS_ODO_OBJECT, ce);
        setStringParm(c.element, CobolMarkup.REDEFINES, ce);
        setBooleanParm(c.element, CobolMarkup.IS_REDEFINED, ce);
        setStringParm(c.element, CobolMarkup.PICTURE, ce);
        setStringParm(c.element, CobolMarkup.USAGE, ce);
        setStringParm(c.element, CobolMarkup.VALUE, ce);
        setBooleanParm(c.element, CobolMarkup.IS_CUSTOM_VARIABLE, ce);
        setStringParm(c.element, CobolMarkup.MARSHAL_CHOICE_STRATEGY, ce);
        setStringParm(c.element, CobolMarkup.UNMARSHAL_CHOICE_STRATEGY, ce);
        setNumericParm(c.element, CobolMarkup.SRCE_LINE, ce);

    }

    /**
     * Move an attribute value from the XML markup to the Cobol annotation.
     * 
     * @param e the Node holding the XML markup
     * @param xmlMarkup the name of the XML markup tag
     * @param ce the target annotation recipient
     */
    private void setBooleanParm(
            final Element e,
            final String xmlMarkup,
            final JAnnotationUse ce) {

        String cobolProperty = xmlMarkup;
        /*
         * TODO There are some differences between the XML markup and
         * the java annotation that need to go away in some future
         * version.
         */
        if (!cobolProperty.startsWith("is")) {
            cobolProperty = "is"
                    + xmlMarkup.substring(0, 1)
                            .toUpperCase(Locale.getDefault())
                    + xmlMarkup.substring(1, xmlMarkup.length());
        }
        String value = e.getAttribute(xmlMarkup);
        if (value == null || value.length() == 0) {
            return;
        }
        ce.param(cobolProperty, Boolean.valueOf(value));
    }

    /**
     * Move an attribute value from the XML markup to the Cobol annotation.
     * 
     * @param e the Node holding the XML markup
     * @param xmlMarkup the name of the XML markup tag
     * @param ce the target annotation recipient
     */
    private void setNumericParm(
            final Element e,
            final String xmlMarkup,
            final JAnnotationUse ce) {

        String cobolProperty = xmlMarkup;
        String value = e.getAttribute(xmlMarkup);
        if (value == null || value.length() == 0) {
            return;
        }
        ce.param(cobolProperty, Integer.valueOf(value));
    }

    /**
     * Move an attribute value from the XML markup to the Cobol annotation.
     * 
     * @param e the Node holding the XML markup
     * @param xmlMarkup the name of the XML markup tag
     * @param ce the target annotation recipient
     */
    private void setStringParm(
            final Element e,
            final String xmlMarkup,
            final JAnnotationUse ce) {

        String cobolProperty = xmlMarkup;
        String value = e.getAttribute(xmlMarkup);
        if (value == null || value.length() == 0) {
            return;
        }
        ce.param(cobolProperty, value);
    }
}
