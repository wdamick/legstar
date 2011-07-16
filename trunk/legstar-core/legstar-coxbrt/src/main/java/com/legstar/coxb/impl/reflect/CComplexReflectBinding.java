/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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
import java.util.List;

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
import com.legstar.coxb.impl.CBinaryBinding;
import com.legstar.coxb.impl.RedefinesMap;
import com.legstar.coxb.util.BindingUtil;
import com.legstar.coxb.util.ClassUtil;

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
    private Object _jaxbObjectFactory;

    /** Java object to which this cobol complex element is bound. */
    private Object _jaxbObject;

    /**
     * Indicates that the associated Jaxb object just came from the constructor
     * and doesn't need to be recreated.
     */
    private boolean _unusedJaxbObject = false;

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
        _jaxbObject = jaxbObject;
        _unusedJaxbObject = true;
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
            final ICobolComplexBinding parentBinding,
            final Object jaxbObjectFactory) throws ReflectBindingException {

        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
        _jaxbObjectFactory = jaxbObjectFactory;
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
     * @param parentJaxbType the parent JAXB Class with annotations
     * @param xmlType the JAXB annotations
     * @throws ReflectBindingException if children bindings fail
     * */
    public void initChildren(final Class < ? > parentJaxbType,
            final XmlType xmlType) throws ReflectBindingException {

        if (_log.isDebugEnabled()) {
            _log.debug("Initializing children of: "
                    + parentJaxbType.getSimpleName());
        }
        /* Map of choice elements for redefined elements */
        RedefinesMap redefinesMap = new RedefinesMap();

        /* Process each property of this complex type in the predefined order */
        for (String prop : xmlType.propOrder()) {

            /* Get a reference to this property field and type */
            Field field;
            Class < ? > jaxbType;
            try {
                field = parentJaxbType.getDeclaredField(prop);
                jaxbType = BindingUtil.getJavaClass(field);

            } catch (SecurityException e) {
                throw new ReflectBindingException(e);
            } catch (NoSuchFieldException e) {
                throw new ReflectBindingException(e);
            } catch (CobolBindingException e) {
                throw new ReflectBindingException(e);
            }

            ICobolBinding cobolBinding = ReflectBindingFactory.createBinding(
                    jaxbType, field, this, _jaxbObjectFactory);

            if (_log.isDebugEnabled()) {
                _log.debug("Java field " + jaxbType.getSimpleName()
                        + " bound to " + cobolBinding);
            }

            /*
             * If this element is a variable size array or list without an
             * explicit depending on clause, dynamically generate a counter.
             */
            if (cobolBinding.getMaxOccurs() > 1
                    && cobolBinding.getMinOccurs() < cobolBinding
                            .getMaxOccurs()
                    && (cobolBinding.getDependingOn() == null || cobolBinding
                            .getDependingOn().length() == 0)) {
                createDynamicCounter(cobolBinding);
            }

            /*
             * If this element is redefined, create a choice which will be
             * populated as we discover alternatives. The choice becomes the
             * parent for the redefined element and all alternatives.
             */
            String redefines = cobolBinding.getRedefines();
            if (cobolBinding.isRedefined()) {
                if (_log.isDebugEnabled()) {
                    _log.debug("Creating Choice binding for redefined Cobol element "
                            + cobolBinding.getCobolName());
                }
                ICobolChoiceBinding choice = ReflectBindingFactory
                        .createChoiceBinding(this, cobolBinding, redefinesMap);
                getChildrenList().add(choice);

            } else if (redefines != null && redefines.length() > 0) {
                if (_log.isDebugEnabled()) {
                    _log.debug("Adding " + cobolBinding.getCobolName()
                            + " to Choice binding for Cobol element "
                            + redefines);
                }
                ICobolChoiceBinding choice = redefinesMap
                        .getChoiceElement(redefines);
                if (choice == null) {
                    throw new ReflectBindingException("Cobol element "
                            + cobolBinding.getCobolName()
                            + " redefining unbound element " + redefines);
                }
                /*
                 * Add the redefining item to the alternative list in the choice
                 * element
                 */
                choice.addAlternative(cobolBinding);

            } else {
                getChildrenList().add(cobolBinding);
            }

        }
        if (_log.isDebugEnabled()) {
            _log.debug("Children sucessfully initialized for: "
                    + parentJaxbType.getSimpleName());
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
        if (_unusedJaxbObject && _jaxbObject != null) {
            _unusedJaxbObject = false;
            return;
        }
        _jaxbObject = BindingUtil.newJaxbObject(_jaxbObjectFactory,
                getJaxbType().getName());
    }

    /** {@inheritDoc} */
    public void setChildrenValues() throws HostException {

        /* Make sure there is an associated JAXB object */
        if (_jaxbObject == null) {
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
                Object value = ClassUtil.invokeGetProperty(_jaxbObject,
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

        ClassUtil.invokeSetProperty(_jaxbObject, child.getJaxbName(), value,
                child.getJaxbType());
    }

    /** {@inheritDoc} */
    public Object getObjectValue(final Class < ? > type) throws HostException {
        if (type.equals(getJaxbType())) {
            return _jaxbObject;
        } else {
            throw new HostException("Attempt to get binding " + getJaxbName()
                    + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
    public void setObjectValue(final Object value) throws HostException {
        if (value == null) {
            _jaxbObject = null;
            return;
        }
        if (value.getClass().equals(getJaxbType())) {
            _jaxbObject = value;
        } else {
            throw new HostException("Attempt to set binding " + getJaxbName()
                    + " from an incompatible value " + value);
        }
    }

    /**
     * @return the java object factory for value objects creation
     */
    public Object getObjectFactory() {
        return _jaxbObjectFactory;
    }

    /**
     * @param objectFactory the java object factory for value objects creation
     */
    public void setObjectFactory(final Object objectFactory) {
        _jaxbObjectFactory = objectFactory;
    }

    /** {@inheritDoc} */
    public boolean isSet() {
        return (_jaxbObject != null);
    }

}
