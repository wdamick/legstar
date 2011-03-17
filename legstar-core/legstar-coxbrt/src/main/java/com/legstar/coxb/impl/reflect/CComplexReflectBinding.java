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
package com.legstar.coxb.impl.reflect;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinaryBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.common.CComplexBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.CArrayBinaryBinding;
import com.legstar.coxb.impl.CArrayDbcsBinding;
import com.legstar.coxb.impl.CArrayDoubleBinding;
import com.legstar.coxb.impl.CArrayFloatBinding;
import com.legstar.coxb.impl.CArrayNationalBinding;
import com.legstar.coxb.impl.CArrayOctetStreamBinding;
import com.legstar.coxb.impl.CArrayPackedDecimalBinding;
import com.legstar.coxb.impl.CArrayStringBinding;
import com.legstar.coxb.impl.CArrayZonedDecimalBinding;
import com.legstar.coxb.impl.CBinaryBinding;
import com.legstar.coxb.impl.CDbcsBinding;
import com.legstar.coxb.impl.CDoubleBinding;
import com.legstar.coxb.impl.CFloatBinding;
import com.legstar.coxb.impl.CNationalBinding;
import com.legstar.coxb.impl.COctetStreamBinding;
import com.legstar.coxb.impl.CPackedDecimalBinding;
import com.legstar.coxb.impl.CStringBinding;
import com.legstar.coxb.impl.CZonedDecimalBinding;
import com.legstar.coxb.impl.RedefinesMap;
import com.legstar.coxb.util.BindingUtil;
import com.legstar.coxb.util.ClassUtil;
import com.legstar.coxb.util.NameUtil;

/**
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. Reflexion is used on the Java object to infer a list of
 * children.
 */

public class CComplexReflectBinding extends CComplexBinding {

    /**
     * Reference to a JAXB object factory. This is needed because this class
     * might need to create JAXB objects.
     */
    private Object mJaxbObjectFactory;

    /** Java object to which this cobol complex element is bound. */
    private Object mJaxbObject;

    /**
     * Indicates that the associated Jaxb object just came from the constructor
     * and doesn't need to be recreated.
     */
    private boolean mUnusedJaxbObject = false;

    /**
     * Dynamic counters are named after the array or list they belong to plus
     * this additional suffix.
     */
    private static final String COUNTER_SUFFIX = "Counter";

    /**
     * Dynamic counters also need a cobol name which is built from the
     * corresponding list or array cobol name plus this suffix.
     */
    private static final String COUNTER_COBOL_SUFFIX = "--C";

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Constructor for a root Complex element with a bound JAXB object.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     * @param jaxbObject the concrete JAXB object instance bound to this complex
     *            element
     * @throws ReflectBindingException if construction fails
     */
    public CComplexReflectBinding(final Object jaxbObjectFactory,
            final Object jaxbObject) throws ReflectBindingException {

        this(jaxbObject.getClass().getSimpleName(), jaxbObject.getClass()
                .getSimpleName(), jaxbObject.getClass(), null, null,
                jaxbObjectFactory);
        mJaxbObject = jaxbObject;
        mUnusedJaxbObject = true;
    }

    /**
     * Constructor for a root Complex element knowing the bound JAXB class.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     * @param jaxbType JAXB type of complex field
     * @throws ReflectBindingException if construction fails
     */
    public CComplexReflectBinding(final Object jaxbObjectFactory,
            final Class < ? > jaxbType) throws ReflectBindingException {

        this(jaxbType.getSimpleName(), jaxbType.getSimpleName(), jaxbType,
                null, null, jaxbObjectFactory);
    }

    /**
     * Constructor for a child Complex element knowing the bound JAXB class.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName field name in parent JAXB object
     * @param jaxbType JAXB type of complex field
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     * @param jaxbObjectFactory the JAXB object factory
     * @throws ReflectBindingException if construction fails
     */
    public CComplexReflectBinding(final String bindingName,
            final String jaxbName, final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final CComplexReflectBinding parentBinding,
            final Object jaxbObjectFactory) throws ReflectBindingException {

        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
        mJaxbObjectFactory = jaxbObjectFactory;
        initComplexElement(jaxbType, jaxbObjectFactory);
    }

    /**
     * Helper method. JAXB Types are annotated with an XmlType which gives an
     * ordered list of properties
     * 
     * @param jaxbType the JAXB Class with annotations
     * @param jaxbObjectFactory the JAXB object factory
     * @throws ReflectBindingException if initialization fails
     */
    private void initComplexElement(final Class < ? > jaxbType,
            final Object jaxbObjectFactory) throws ReflectBindingException {

        if (_log.isDebugEnabled()) {
            _log.debug("Initializing Complex binding for " + jaxbType);
        }
        XmlType xmlType = (XmlType) jaxbType.getAnnotation(XmlType.class);
        if (xmlType == null) {
            throw new ReflectBindingException("No jaxb annotations found in "
                    + jaxbType);
        }
        if (_log.isDebugEnabled()) {
            _log.debug("Found JAXB annotations: " + xmlType.toString());
        }

        /* Assume we are bound to a JAXB object */
        setValueObjectClassName(jaxbType.getName());
        setValueObjectsFactoryClassName(jaxbObjectFactory.getClass().getName());

        /*
         * Jaxb class might hold an annotation which gives more details on how
         * to bind
         */
        CobolComplexType cobolComplexType = (CobolComplexType) jaxbType
                .getAnnotation(CobolComplexType.class);
        if (cobolComplexType != null
                && cobolComplexType.javaClassName() != null
                && cobolComplexType.javaClassName().length() > 0) {
            setValueObjectClassName(cobolComplexType.javaClassName());
            /*
             * TODO allow more options, such as factory name, to be passed as
             * annotations
             */
            setValueObjectsFactoryClassName(null);
        }

        initChildren(jaxbType, xmlType);

        if (_log.isDebugEnabled()) {
            _log.debug("Complex binding sucessfully initialized for: "
                    + jaxbType);
        }
    }

    /**
     * Creates a binding property for each child.
     * 
     * @param jaxbType the JAXB Class with annotations
     * @param xmlType the JAXB annotations
     * @throws ReflectBindingException if children bindings fail
     * */
    public void initChildren(final Class < ? > jaxbType, final XmlType xmlType)
            throws ReflectBindingException {

        if (_log.isDebugEnabled()) {
            _log.debug("Initializing children of: " + jaxbType.getSimpleName());
        }
        /* Map of choice elements for redefined elements */
        RedefinesMap redefinesMap = new RedefinesMap();

        /* Process each property of this complex type in the predefined order */
        for (String prop : xmlType.propOrder()) {

            /* Get a reference to this property field and type */
            Field hostField;
            try {
                hostField = jaxbType.getDeclaredField(prop);
            } catch (SecurityException e) {
                throw new ReflectBindingException(e);
            } catch (NoSuchFieldException e) {
                throw new ReflectBindingException(e);
            }

            /* Get the cobol annotations for that field */
            CobolElement cobolAnnotations = hostField
                    .getAnnotation(CobolElement.class);
            if (cobolAnnotations == null) {
                throw new ReflectBindingException(
                        "No cobol annotations found for field "
                                + hostField.getName());
            }

            if (_log.isDebugEnabled()) {
                _log.debug("Processing Cobol annotations for: "
                        + cobolAnnotations.cobolName());
                _log.debug("Cobol annotations: " + cobolAnnotations);
            }

            String jaxbName = getJaxbName(jaxbType, hostField,
                    cobolAnnotations.maxOccurs());

            ICobolBinding binding;
            try {
                binding = createBinding(jaxbName,
                        BindingUtil.getJavaClass(hostField), cobolAnnotations,
                        redefinesMap);
            } catch (CobolBindingException e) {
                throw new ReflectBindingException(e);
            }
            /*
             * In the case of redefines, no actual binding is created for a
             * redefining item since all alternatives share the same choice
             * binding. Hence the possibility of a null.
             */
            if (binding != null) {
                getChildrenList().add(binding);
            }

        }
        if (_log.isDebugEnabled()) {
            _log.debug("Children sucessfully initialized for: "
                    + jaxbType.getSimpleName());
        }
    }

    /**
     * Get the JAXB property name usable by appending "get".
     * <p/>
     * We first try the host field name processed with the JAXB name converter
     * which strips off underscores, etc... This is the most likely situation.
     * <p/>
     * If first try does not work, we try the host field name directly. This
     * case corresponds to ECI compatibility mode which bypasses the JAXB name
     * converter.
     * <p/>
     * If this still does not work, we try the XmlElement annotation. This
     * corresponds to cases (@see numzoned) where XmlEelement annotation is used
     * to override the proposed JAXB name.
     * 
     * @param jaxbType the parent JAXB object
     * @param hostField the JAXB field
     * @param maxOccurs the number of occurrences (needed to identify lists)
     * @return a property name usable to create a getter a method on the JAXB
     *         object
     * @throws ReflectBindingException
     */
    protected String getJaxbName(Class < ? > jaxbType, final Field hostField,
            final int maxOccurs) throws ReflectBindingException {

        Method getter = null;
        String getterPrefix = ClassUtil.getGetterPrefix(hostField.getType(),
                maxOccurs);

        try {
            getter = jaxbType.getMethod(getGetterName(getterPrefix,
                    NameUtil.toClassName(hostField.getName())));
        } catch (SecurityException e) {
            throw new ReflectBindingException(e);
        } catch (NoSuchMethodException e) {
            try {
                getter = jaxbType.getMethod(getGetterName(getterPrefix,
                        hostField.getName()));
            } catch (SecurityException e1) {
                throw new ReflectBindingException(e1);
            } catch (NoSuchMethodException e1) {
                XmlElement xmlAnnotation = hostField
                        .getAnnotation(XmlElement.class);
                if (xmlAnnotation != null && xmlAnnotation.name() != null
                        && !xmlAnnotation.name().equals("##default")) {
                    try {
                        getter = jaxbType.getMethod(getGetterName(getterPrefix,
                                NameUtil.toClassName(xmlAnnotation.name())));
                    } catch (SecurityException e2) {
                        throw new ReflectBindingException(e);
                    } catch (NoSuchMethodException e2) {
                        throw new ReflectBindingException(e);
                    }
                } else {
                    throw new ReflectBindingException(
                            "Unable to find JAXB getter method for "
                                    + hostField.getName());
                }
            }
        }
        return getter.getName().substring(getterPrefix.length());

    }

    /**
     * Build a conventional bean getter method name from a field name.
     * 
     * @param getterPrefix the getter method name prefix (get or is)
     * @param fieldName the field name
     * @return a getter method name
     */
    protected String getGetterName(final String getterPrefix,
            final String fieldName) {
        return getterPrefix + Character.toUpperCase(fieldName.charAt(0))
                + fieldName.substring(1);
    }

    /**
     * Based on the cobol annotations for that field, we create a corresponding
     * element type. If this element is redefined we create a choice element and
     * return it instead of the the redefined element itself.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param redefinesMap the current list of redefined items
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    protected ICobolBinding createBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final RedefinesMap redefinesMap) throws ReflectBindingException {

        if (_log.isDebugEnabled()) {
            _log.debug("Binding cobol element " + cobolAnnotations.cobolName()
                    + '(' + cobolAnnotations.type() + ')'
                    + " to java property " + jaxbName + '('
                    + jaxbType.getName() + ')');
        }
        ICobolBinding cobolElement = null;

        switch (cobolAnnotations.type()) {
        case GROUP_ITEM:
            cobolElement = createComplexBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case ALPHABETIC_ITEM:
        case ALPHANUMERIC_EDITED_ITEM:
        case ALPHANUMERIC_ITEM:
        case NUMERIC_EDITED_ITEM:
        case EXTERNAL_FLOATING_ITEM:
            cobolElement = createStringBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case NATIONAL_ITEM:
            cobolElement = createNationalBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case PACKED_DECIMAL_ITEM:
            cobolElement = createPackedDecimalBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case ZONED_DECIMAL_ITEM:
            cobolElement = createZonedDecimalBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case DBCS_ITEM:
            cobolElement = createDbcsBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case OCTET_STREAM_ITEM:
        case INDEX_ITEM:
        case POINTER_ITEM:
        case PROC_POINTER_ITEM:
        case FUNC_POINTER_ITEM:
            cobolElement = createOctetStreamBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case BINARY_ITEM:
        case NATIVE_BINARY_ITEM:
            cobolElement = createBinaryBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case SINGLE_FLOAT_ITEM:
            cobolElement = createFloatBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        case DOUBLE_FLOAT_ITEM:
            cobolElement = createDoubleBinding(jaxbName, jaxbType,
                    cobolAnnotations);
            break;
        default:
            throw (new ReflectBindingException(
                    "Unrecognized cobol type for field "
                            + cobolAnnotations.cobolName()));
        }

        /*
         * If this element is a variable size array or list without an explicit
         * depending on clause, dynamically generate a counter.
         */
        if (cobolAnnotations.maxOccurs() > 1
                && cobolAnnotations.minOccurs() < cobolAnnotations.maxOccurs()
                && (cobolAnnotations.dependingOn() == null || cobolAnnotations
                        .dependingOn().length() == 0)) {
            createDynamicCounter(cobolElement);
        }

        /*
         * If this element is part of a redefinition group (either redefines
         * another element or is redefined by another element) we need further
         * processing.
         */
        String redefines = cobolAnnotations.redefines();
        if ((cobolAnnotations.isRedefined())
                || (redefines != null && redefines.length() > 0)) {
            return createChoiceBinding(jaxbName, cobolAnnotations,
                    cobolElement, redefinesMap);
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Binding created " + cobolElement.getBindingName());
        }
        return cobolElement;
    }

    /**
     * Create a group element type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createComplexBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        /* JAXB considers a member to be an array only if maxOccurs > 1 */
        if (cobolAnnotations.maxOccurs() > 1) {
            /* A single complex binding is used for all items */
            ICobolComplexBinding item = new CComplexReflectBinding(jaxbName,
                    jaxbName, jaxbType, cobolAnnotations, this,
                    mJaxbObjectFactory);
            return new CArrayComplexReflectBinding(jaxbName + "Wrapper",
                    jaxbName, jaxbType, cobolAnnotations, this, item,
                    mJaxbObjectFactory);
        } else {
            return new CComplexReflectBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this, mJaxbObjectFactory);
        }
    }

    /**
     * Create a choice element type which will group all alternatives. The
     * choice element replaces the redefined element in the parent hierarchy.
     * All alternative (redefining elements) are then added to the choice.
     * 
     * @param jaxbName the java property name
     * @param cobolAnnotations the cobol annotations for this element
     * @param cobolElement the cobol descriptor for this element
     * @param redefinesMap the current list of redefined items
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol binding cannot be created
     */
    private ICobolBinding createChoiceBinding(final String jaxbName,
            final CobolElement cobolAnnotations,
            final ICobolBinding cobolElement, final RedefinesMap redefinesMap)
            throws ReflectBindingException {

        if (_log.isDebugEnabled()) {
            if (cobolAnnotations.isRedefined()) {
                _log.debug("Creating Choice binding for redefined Cobol element "
                        + cobolAnnotations.cobolName());
            } else {
                _log.debug("Adding " + cobolAnnotations.cobolName()
                        + " to Choice binding for Cobol element "
                        + cobolAnnotations.redefines());
            }
        }
        /*
         * If this is a redefined item, then we need to create a special choice
         * element which will group all alternatives. The identifier of this
         * choice element is built from the first alternative java name.
         */
        if (cobolAnnotations.isRedefined()) {

            CChoiceReflectBinding choice = new CChoiceReflectBinding(jaxbName
                    + "Choice", cobolAnnotations, this);

            /*
             * Add the redefined item as the first alternative in the choice
             * element
             */
            choice.addAlternative(cobolElement);

            /* Add this choice element in the redefines map */
            redefinesMap.updateChoiceElement(cobolAnnotations.cobolName(),
                    choice);

            if (_log.isDebugEnabled()) {
                _log.debug("Choice binding created");
            }
            /* Return the choice as the current element for caller */
            return choice;

        }

        /*
         * This is a redefinition of an existing element, we need to add this
         * alternative to the redefined element list of alternatves
         */
        /*
         * lookup the redefines Map to locate the choice element we are part of
         */
        ICobolChoiceBinding choice = redefinesMap
                .getChoiceElement(cobolAnnotations.redefines());
        if (choice == null) {
            /*
             * If the redefined element has not been processed, this means it
             * was not created by the caller (he already made a choice). In this
             * case, the current element is the first of the possible
             * alternatives.
             */
            choice = new CChoiceReflectBinding(jaxbName + "Choice",
                    cobolAnnotations, this);

            /*
             * Add the redefined item as the first alternative in the choice
             * element
             */
            choice.addAlternative(cobolElement);

            /*
             * Add this choice element in the redefines map under the name of
             * the redefined element.
             */
            redefinesMap.updateChoiceElement(cobolAnnotations.redefines(),
                    choice);

            /* Return the choice as the current element for caller */
            return choice;
        }

        /*
         * Add the redefining item to the alternative list in the choice element
         */
        choice.addAlternative(cobolElement);

        if (_log.isDebugEnabled()) {
            _log.debug("Choice binding updated");
        }
        /*
         * Since choice element is already part of parent children, return null
         * to avoid adding it twice
         */
        return null;

    }

    /**
     * Create a String binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createStringBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayStringBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CStringBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a OctetStream binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createOctetStreamBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayOctetStreamBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new COctetStreamBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a National binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createNationalBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayNationalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CNationalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a Dbcs binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createDbcsBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayDbcsBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CDbcsBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a PackedDecimal binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createPackedDecimalBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayPackedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CPackedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a ZonedDecimal binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createZonedDecimalBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayZonedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CZonedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a Binary binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createBinaryBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayBinaryBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CBinaryBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a Float binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createFloatBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayFloatBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CFloatBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Create a Double binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinding createDoubleBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayDoubleBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        } else {
            return new CDoubleBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, this);
        }
    }

    /**
     * Generates a transient binding to hold the item counter for a variable
     * size array when no explicit depending on clause exist. This sitution
     * arises when the jaxb object was generated from an XSD instead of an
     * existing cobol copybook.
     * 
     * @param cobolElement the variable size array or list
     * @throws ReflectBindingException if counter cannot be created
     */
    private void createDynamicCounter(final ICobolBinding cobolElement)
            throws ReflectBindingException {

        ICobolBinaryBinding counter = createDynamicCounterBinding(cobolElement);
        storeCounter(counter);

        /*
         * Now inform the variable size array that it has a depending on object
         */
        cobolElement.setDependingOn(counter.getCobolName());

        /*
         * Arrays of complex items have a reference to a single binding used for
         * all items. That binding holds the same cobol annotations as the
         * wrapper array. Since we updated the wrapper depending on clause, we
         * need to do the same at the item level.
         */
        if (cobolElement instanceof ICobolArrayComplexBinding) {
            ((ICobolArrayComplexBinding) cobolElement).getComplexItemBinding()
                    .setDependingOn(counter.getCobolName());
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Created depending on relationship for "
                    + cobolElement.getBindingName() + " with "
                    + counter.getCobolName());
        }
    }

    /**
     * Variable size arrays and lists need an extra numeric element to count
     * items. Such element is a special "transient" binding which has no
     * associated jaxb property.
     * 
     * @param listBinding the cobol binding for the array or list
     * @return the counter binding
     * @throws ReflectBindingException if cobol description cannot be created
     */
    private ICobolBinaryBinding createDynamicCounterBinding(
            final ICobolBinding listBinding) throws ReflectBindingException {
        if (_log.isDebugEnabled()) {
            _log.debug("Creating a dynamic counter for "
                    + listBinding.getBindingName());
        }
        CBinaryBinding counter = new CBinaryBinding(
                listBinding.getBindingName() + COUNTER_SUFFIX, null, null,
                null, this);
        counter.setCobolName(getCounterCobolName(listBinding.getCobolName()));
        counter.setLevelNumber(listBinding.getLevelNumber());
        counter.setUsage("BINARY");
        counter.setPicture("9(9)");
        counter.setByteLength(4);
        counter.setTotalDigits(9);
        counter.setIsODOObject(true);
        return counter;
    }

    /**
     * Dynamic counters need a unique Cobol name. This method determines such a
     * name based on the related array or list cobol name. This method does not
     * guarantee unicity. TODO reuse logic in CobolGen for unique Cobol name
     * generation
     * 
     * @param cobolName cobol name of corresponding list or array
     * @return the proposed counter cobol name
     */
    private String getCounterCobolName(final String cobolName) {
        if (cobolName.length() < 31 - COUNTER_COBOL_SUFFIX.length()) {
            return cobolName + COUNTER_COBOL_SUFFIX;
        } else {
            return cobolName.substring(0, 30 - COUNTER_COBOL_SUFFIX.length())
                    + COUNTER_COBOL_SUFFIX;
        }
    }

    /** {@inheritDoc} */
    public void createJaxbObject() throws HostException {
        createValueObject();
    }

    /** {@inheritDoc} */
    public void createValueObject() throws HostException {
        /*
         * Since this complex binding has a constructor that takes a value
         * object, we might already have a value object that was not used yet.
         */
        if (mUnusedJaxbObject && mJaxbObject != null) {
            mUnusedJaxbObject = false;
            return;
        }
        mJaxbObject = BindingUtil.newJaxbObject(mJaxbObjectFactory,
                getJaxbType().getName());
    }

    /** {@inheritDoc} */
    public void setChildrenValues() throws HostException {

        /* Make sure there is an associated JAXB object */
        if (mJaxbObject == null) {
            createJaxbObject();
        }

        /* Set this binding properties from java object property values */
        for (ICobolBinding child : getChildrenList()) {
            /*
             * Children that are not bound to a jaxb property are ignored. This
             * includes Choices and dynamically generated counbters for
             * instance.
             */
            if (!child.isBound()) {
                continue;
            } else {
                Object value = ClassUtil.invokeGetProperty(mJaxbObject,
                        child.getJaxbName(), child.getJaxbType(),
                        child.getMaxOccurs());
                if (_log.isDebugEnabled()) {
                    _log.debug("Getting value from JAXB property "
                            + child.getJaxbName() + " value=" + value);
                }
                child.setObjectValue(value);

                /*
                 * If this is a variable size array or list, make sure any
                 * associated counter is updated
                 */
                if (child.getMaxOccurs() > 1
                        && child.getMinOccurs() < child.getMaxOccurs()) {
                    setCounterValue(child.getDependingOn(),
                            ((List < ? >) value).size());
                }
            }
        }
    }

    /** {@inheritDoc} */
    public void setJaxbPropertyValue(final int index) throws HostException {
        setPropertyValue(index);
    }

    /** {@inheritDoc} */
    public void setPropertyValue(final int index) throws HostException {

        ICobolBinding child = getChildrenList().get(index);

        /*
         * Children that are not bound to a value object are ignored. This
         * includes Choices and dynamically generated counters for instance.
         */
        if (!child.isBound()) {
            return;
        }

        Object value = child.getObjectValue(child.getJaxbType());
        if (_log.isDebugEnabled()) {
            _log.debug("Setting value of JAXB property " + child.getJaxbName()
                    + " value=" + value);
        }

        ClassUtil.invokeSetProperty(mJaxbObject, child.getJaxbName(), value,
                child.getJaxbType());
    }

    /** {@inheritDoc} */
    public Object getObjectValue(final Class < ? > type) throws HostException {
        if (type.equals(getJaxbType())) {
            return mJaxbObject;
        } else {
            throw new HostException("Attempt to get binding " + getJaxbName()
                    + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
    public void setObjectValue(final Object value) throws HostException {
        if (value == null) {
            mJaxbObject = null;
            return;
        }
        if (value.getClass().equals(getJaxbType())) {
            mJaxbObject = value;
        } else {
            throw new HostException("Attempt to set binding " + getJaxbName()
                    + " from an incompatible value " + value);
        }
    }

    /**
     * @return the java object factory for value objects creation
     */
    public Object getObjectFactory() {
        return mJaxbObjectFactory;
    }

    /**
     * @param objectFactory the java object factory for value objects creation
     */
    public void setObjectFactory(final Object objectFactory) {
        mJaxbObjectFactory = objectFactory;
    }

    /** {@inheritDoc} */
    public boolean isSet() {
        return (mJaxbObject != null);
    }

}
