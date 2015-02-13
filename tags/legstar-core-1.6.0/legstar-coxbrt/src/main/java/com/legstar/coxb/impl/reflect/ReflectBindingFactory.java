/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.xml.bind.annotation.XmlElement;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolJavaTypeAdapter;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
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
import com.legstar.coxb.util.ClassUtil;
import com.legstar.coxb.util.NameUtil;

/**
 * Creates specialized COBOL/Java bindings based upon properties discovered
 * using reflection on JAXB classes.
 * 
 */
public final class ReflectBindingFactory {

    /**
     * Choice bindings derive their name from the redefined alternative.
     */
    private static final String CHOICE_NAME_SUFFIX = "Choice";

    /**
     * Complex array bindings derive their name from the occuring complex item.
     */
    private static final String COMPLEX_ARRAY_NAME_SUFFIX = "Wrapper";

    /**
     * Utility class.
     */
    private ReflectBindingFactory() {

    }

    /**
     * Create a specialized binding by analyzing a JAXB class field.
     * <p/>
     * The strategy is as follows:
     * <ul>
     * <li>If there is an explicit adapter, then it becomes the binding</li>
     * <li>Otherwise binding is inferred from COBOL annotations</li>
     * </ul>
     * 
     * @param jaxbType the java property type
     * @param field the JAXB class field
     * @param parentBinding the parent binding (null if none)
     * @param jaxbObjectFactory the JAXB object factory
     * @return a specialized binding
     */
    public static ICobolBinding createBinding(final Class < ? > jaxbType,
            final Field field, final ICobolComplexBinding parentBinding,
            final Object jaxbObjectFactory) throws ReflectBindingException {

        CobolElement cobolAnnotations = field.getAnnotation(CobolElement.class);
        if (cobolAnnotations == null) {
            throw new ReflectBindingException(
                    "No cobol annotations found for field " + field.getName());
        }
        String jaxbName = getJaxbName(parentBinding.getJaxbType(), field,
                cobolAnnotations.maxOccurs());

        CobolJavaTypeAdapter adapter = field
                .getAnnotation(CobolJavaTypeAdapter.class);
        if (adapter != null) {
            return getCustomAdapter(adapter, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }

        return createBinding(jaxbName, jaxbType, cobolAnnotations,
                parentBinding, jaxbObjectFactory);

    }

    /**
     * Dispatch creation methods based on the COBOL type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the COBOL annotations
     * @param parentBinding the parent binding (null if none)
     * @param jaxbObjectFactory the JAXB object factory
     * @return a specialized binding
     * @throws ReflectBindingException
     */
    public static ICobolBinding createBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding,
            final Object jaxbObjectFactory) throws ReflectBindingException {

        switch (cobolAnnotations.type()) {
        case GROUP_ITEM:
            return createComplexBinding(jaxbName, jaxbType, cobolAnnotations,
                    parentBinding, jaxbObjectFactory);
        case ALPHABETIC_ITEM:
        case ALPHANUMERIC_EDITED_ITEM:
        case ALPHANUMERIC_ITEM:
        case NUMERIC_EDITED_ITEM:
        case EXTERNAL_FLOATING_ITEM:
            return createStringBinding(jaxbName, jaxbType, cobolAnnotations,
                    parentBinding);
        case NATIONAL_ITEM:
            return createNationalBinding(jaxbName, jaxbType, cobolAnnotations,
                    parentBinding);
        case PACKED_DECIMAL_ITEM:
            return createPackedDecimalBinding(jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        case ZONED_DECIMAL_ITEM:
            return createZonedDecimalBinding(jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        case DBCS_ITEM:
            return createDbcsBinding(jaxbName, jaxbType, cobolAnnotations,
                    parentBinding);
        case OCTET_STREAM_ITEM:
        case INDEX_ITEM:
        case POINTER_ITEM:
        case PROC_POINTER_ITEM:
        case FUNC_POINTER_ITEM:
            return createOctetStreamBinding(jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        case BINARY_ITEM:
        case NATIVE_BINARY_ITEM:
            return createBinaryBinding(jaxbName, jaxbType, cobolAnnotations,
                    parentBinding);
        case SINGLE_FLOAT_ITEM:
            return createFloatBinding(jaxbName, jaxbType, cobolAnnotations,
                    parentBinding);
        case DOUBLE_FLOAT_ITEM:
            return createDoubleBinding(jaxbName, jaxbType, cobolAnnotations,
                    parentBinding);
        default:
            throw (new ReflectBindingException(
                    "Unrecognized cobol type for field "
                            + cobolAnnotations.cobolName()));
        }

    }

    /**
     * Create a group element type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding (null if none)
     * @param jaxbObjectFactory the JAXB object factory
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createComplexBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding,
            final Object jaxbObjectFactory) throws ReflectBindingException {

        /* JAXB considers a member to be an array only if maxOccurs > 1 */
        if (cobolAnnotations.maxOccurs() > 1) {
            /* A single complex binding is used for all items */
            ICobolComplexBinding item = new CComplexReflectBinding(jaxbName,
                    jaxbName, jaxbType, cobolAnnotations, parentBinding,
                    jaxbObjectFactory);
            return new CArrayComplexReflectBinding(jaxbName
                    + COMPLEX_ARRAY_NAME_SUFFIX, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding, item, jaxbObjectFactory);
        } else {
            return new CComplexReflectBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding, jaxbObjectFactory);
        }
    }

    /**
     * Create a choice element type which will group all alternatives.
     * <p/>
     * The identifier of this choice element is built from the first alternative
     * java name.
     * <p/>
     * The choice element replaces the redefined element in the parent
     * hierarchy. All alternative (redefining elements) are then added to the
     * choice.
     * 
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding (null if none)
     * @param cobolBinding the cobol descriptor for this element
     * @param redefinesMap the current list of redefined items
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol binding cannot be created
     */
    public static ICobolChoiceBinding createChoiceBinding(
            final ICobolComplexBinding parentBinding,
            final ICobolBinding cobolBinding, final RedefinesMap redefinesMap)
            throws ReflectBindingException {

        String choiceJaxbName = cobolBinding.getJaxbName() + CHOICE_NAME_SUFFIX;

        CChoiceReflectBinding choice = new CChoiceReflectBinding(
                choiceJaxbName, parentBinding);

        /*
         * Propagate unmarshaling/marshaling strategies from first alternative
         * to the new choice element.
         */
        choice.setMarshalChoiceStrategyClassName(cobolBinding
                .getMarshalChoiceStrategyClassName());
        choice.setUnmarshalChoiceStrategyClassName(cobolBinding
                .getUnmarshalChoiceStrategyClassName());
        choice.setCobolName(cobolBinding.getCobolName());

        /*
         * Add the redefined item as the first alternative in the choice element
         */
        choice.addAlternative(cobolBinding);

        /* Add this choice element in the redefines map */
        redefinesMap.updateChoiceElement(cobolBinding.getCobolName(), choice);

        /* Return the choice as the current element for caller */
        return choice;

    }

    /**
     * Create a String binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createStringBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayStringBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CStringBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a OctetStream binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createOctetStreamBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayOctetStreamBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new COctetStreamBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a National binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createNationalBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayNationalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CNationalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a Dbcs binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createDbcsBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayDbcsBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CDbcsBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a PackedDecimal binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createPackedDecimalBinding(
            final String jaxbName, final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayPackedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CPackedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a ZonedDecimal binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createZonedDecimalBinding(
            final String jaxbName, final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayZonedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CZonedDecimalBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a Binary binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createBinaryBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayBinaryBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CBinaryBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a Float binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createFloatBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayFloatBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CFloatBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        }
    }

    /**
     * Create a Double binding type.
     * 
     * @param jaxbName the java property name
     * @param jaxbType the java property type
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding the parent binding
     * @return the new cobol element description
     * @throws ReflectBindingException if cobol description cannot be created
     */
    public static ICobolBinding createDoubleBinding(final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {

        if (cobolAnnotations.maxOccurs() > 0) {
            return new CArrayDoubleBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } else {
            return new CDoubleBinding(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
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
     * @param parentJaxbType the parent JAXB object type
     * @param hostField the JAXB field
     * @param maxOccurs the number of occurrences (needed to identify lists)
     * @return a property name usable to create a getter a method on the JAXB
     *         object
     * @throws ReflectBindingException
     */
    public static String getJaxbName(Class < ? > parentJaxbType,
            final Field hostField, final int maxOccurs)
            throws ReflectBindingException {

        Method getter = null;
        String getterPrefix = ClassUtil.getGetterPrefix(hostField.getType(),
                maxOccurs);

        try {
            getter = parentJaxbType.getMethod(ClassUtil.getGetterName(
                    getterPrefix, NameUtil.toClassName(hostField.getName())));
        } catch (SecurityException e) {
            throw new ReflectBindingException(e);
        } catch (NoSuchMethodException e) {
            try {
                getter = parentJaxbType.getMethod(ClassUtil.getGetterName(
                        getterPrefix, hostField.getName()));
            } catch (SecurityException e1) {
                throw new ReflectBindingException(e1);
            } catch (NoSuchMethodException e1) {
                XmlElement xmlAnnotation = hostField
                        .getAnnotation(XmlElement.class);
                if (xmlAnnotation != null && xmlAnnotation.name() != null
                        && !xmlAnnotation.name().equals("##default")) {
                    try {
                        getter = parentJaxbType.getMethod(ClassUtil
                                .getGetterName(getterPrefix, NameUtil
                                        .toClassName(xmlAnnotation.name())));
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
     * Elements might be associated with a custom adapter.
     * <p/>
     * Custom adapters will usually inherit from
     * {@link com.legstar.coxb.common.CBinding} or one of its subclasses.
     * <p/>
     * The adapter must implement a constructor that takes the same parameters
     * as CBinding.
     * 
     * @param adapter the adapter annotations
     * @param jaxbName the JAXB name of the bound property
     * @param jaxbType the JAXB type of the bound property
     * @param cobolAnnotations the COBOL annotations
     * @param parentBinding the parent binding
     * @return an instance of the adapter
     * @throws ReflectBindingException if instantiation fails
     */
    protected static ICobolBinding getCustomAdapter(
            CobolJavaTypeAdapter adapter, final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding)
            throws ReflectBindingException {
        try {
            Class < ? extends ICobolBinding > adapterClass = adapter.value();
            Constructor < ? extends ICobolBinding > ctor = adapterClass
                    .getConstructor(String.class, String.class, Class.class,
                            CobolElement.class, ICobolComplexBinding.class);
            return ctor.newInstance(jaxbName, jaxbName, jaxbType,
                    cobolAnnotations, parentBinding);
        } catch (SecurityException e) {
            throw new ReflectBindingException(e);
        } catch (NoSuchMethodException e) {
            throw new ReflectBindingException(e);
        } catch (IllegalArgumentException e) {
            throw new ReflectBindingException(e);
        } catch (InstantiationException e) {
            throw new ReflectBindingException(e);
        } catch (IllegalAccessException e) {
            throw new ReflectBindingException(e);
        } catch (InvocationTargetException e) {
            throw new ReflectBindingException(e);
        }

    }

}
