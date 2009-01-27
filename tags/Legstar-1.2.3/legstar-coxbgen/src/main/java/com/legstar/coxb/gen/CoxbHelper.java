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
package com.legstar.coxb.gen;

import java.util.Locale;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolBinaryBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.util.JaxbUtil;

/**
 * Provides the generator with convenience methods. The class can be 
 * passed as an instance to the velocity engine and used by templates.
 * JaxbUtil itself cannot be passed because it is static and cannot be
 * instantiated.
 * 
 */
public class CoxbHelper {

    /** Package name for all binding interfaces. */
    private static final String COXB_INTERFACES_PKGNAME = "com.legstar.coxb";

    /** Used for all java fields. Useful in case generated field names happen
     * to be java reserved words. */
    private static final String FIELD_NAMES_PREFIX = "_";

    /**
     * Builds a binding type name using the associated jaxb type name.
     * @param binding the binding for which the binding type is to be returned
     * @return the binding type name
     */
    public final String getCoxbTypeName(final ICobolBinding binding) {
        return JaxbUtil.getCoxbTypeName(binding);
    }

    /**
     * Get the binding type for the inner item of a complex array.
     * @param binding the binding for which the binding type is to be returned
     * @return the binding type name
     */
    public final String getItemCoxbTypeName(
            final ICobolArrayComplexBinding binding) {
        return JaxbUtil.getCoxbTypeName(binding.getComplexItemBinding());
    }

    /**
     * Within velocity templates this helps determine the classes to include.
     * @param binding a bound element
     * @return the name of the interface this bound element implements
     */
    public final String getBindingInterfaceName(final ICobolBinding binding) {
        if (binding instanceof ICobolComplexBinding) {
            return "ICobolComplexBinding";
        }
        if (binding instanceof ICobolChoiceBinding) {
            return "ICobolChoiceBinding";
        }
        if (binding instanceof ICobolArrayComplexBinding) {
            return "ICobolArrayComplexBinding";
        }
        if (binding instanceof ICobolStringBinding) {
            return "ICobolStringBinding";
        }
        if (binding instanceof ICobolArrayStringBinding) {
            return "ICobolArrayStringBinding";
        }
        if (binding instanceof ICobolNationalBinding) {
            return "ICobolNationalBinding";
        }
        if (binding instanceof ICobolArrayNationalBinding) {
            return "ICobolArrayNationalBinding";
        }
        if (binding instanceof ICobolZonedDecimalBinding) {
            return "ICobolZonedDecimalBinding";
        }
        if (binding instanceof ICobolArrayZonedDecimalBinding) {
            return "ICobolArrayZonedDecimalBinding";
        }
        if (binding instanceof ICobolPackedDecimalBinding) {
            return "ICobolPackedDecimalBinding";
        }
        if (binding instanceof ICobolArrayPackedDecimalBinding) {
            return "ICobolArrayPackedDecimalBinding";
        }
        if (binding instanceof ICobolBinaryBinding) {
            return "ICobolBinaryBinding";
        }
        if (binding instanceof ICobolArrayBinaryBinding) {
            return "ICobolArrayBinaryBinding";
        }
        if (binding instanceof ICobolFloatBinding) {
            return "ICobolFloatBinding";
        }
        if (binding instanceof ICobolArrayFloatBinding) {
            return "ICobolArrayFloatBinding";
        }
        if (binding instanceof ICobolDoubleBinding) {
            return "ICobolDoubleBinding";
        }
        if (binding instanceof ICobolArrayDoubleBinding) {
            return "ICobolArrayDoubleBinding";
        }
        if (binding instanceof ICobolOctetStreamBinding) {
            return "ICobolOctetStreamBinding";
        }
        if (binding instanceof ICobolArrayOctetStreamBinding) {
            return "ICobolArrayOctetStreamBinding";
        }
        return null;
    }

    /**
     * Returns fully qualified interface class name to use in import statements.
     * @param binding a bound element
     * @return the fully qualified name of the interface this bound element
     *  implements
     */
    public final String getQualifiedBindingInterfaceName(
            final ICobolBinding binding) {
        return COXB_INTERFACES_PKGNAME + '.' + getBindingInterfaceName(binding);
    }

    /**
     * Simple types instances are created using a factory. A method
     * name in the factory matches each binding interface name.
     * @param binding a bound element
     * @return the create method name from the factory for this type
     */
    public final String getCreateMethod(final ICobolBinding binding) {
        String interfaceName = getBindingInterfaceName(binding);
        return interfaceName.substring(
                interfaceName.indexOf("ICobol") + "ICobol".length(),
                interfaceName.length());
    }

    /**
     * Returns a generic type to simplify code generation.
     * @param binding a bound element
     * @return a generic type
     */
    public final String getGenericType(final ICobolBinding binding) {
        if (binding instanceof ICobolComplexBinding) {
            return "complex";
        }
        if (binding instanceof ICobolChoiceBinding) {
            return "choice";
        }
        if (binding instanceof ICobolArrayComplexBinding) {
            return "complexArray";
        }
        if (JaxbUtil.isEnum(JaxbUtil.getJaxbTypeName(binding))) {
            return "enum";
        } else {
            return "simple";
        }
    }

    /**
     * @return the binding interfaces package name
     */
    public final String getCoxbPackageName() {
        return COXB_INTERFACES_PKGNAME;
    }

    /**
     * Retrieve the bound object type. Since there are more
     * precise methods for complex objects, this will apply for
     * simple objects.
     * @param binding a binding element
     * @return the bound object type name
     */
    public final String getBoundTypeName(final ICobolBinding binding) {
        return JaxbUtil.getJaxbTypeName(binding);
    }

    /**
     * Retrieve the bound object type. Complex objects can be bound t
     * JAXB objects or straight POJOs.
     * @param binding a complex element
     * @return the bound object type name
     */
    public final String getBoundTypeName(final ICobolComplexBinding binding) {
        return new CodeGenHelper().getClassName(
                binding.getValueObjectClassName());
    }

    /**
     * Choices do not have a bound object but they always belong to
     * a complex parent that does.
     * @param binding a choice element
     * @return the parent's bound object type name
     */
    public final String getBoundTypeName(
            final ICobolChoiceBinding binding) {
        return getBoundTypeName(binding.getParentBinding());
    }

    /**
     * Complex array inner item bound object.
     * @param binding a complex array element
     * @return the items bound object type name
     */
    public final String getItemBoundTypeName(
            final ICobolArrayComplexBinding binding) {
        return getBoundTypeName(binding.getComplexItemBinding());
    }

    /**
     * @param binding a bound element
     * @return the jaxb variable name of the element
     */
    public final String getFieldName(final ICobolBinding binding) {
        return FIELD_NAMES_PREFIX + JaxbUtil.getFieldName(binding);
    }

    /**
     * Builds a get method name for a field name. The get method must
     * be a valid Jaxb method.
     * @param binding a bound element
     * @return a getter method
     */
    public final String getterMethodName(final ICobolBinding binding) {
        /* Jaxb objects export lists rather than wrappers */
        if (binding instanceof ICobolArrayComplexBinding) {
            return getterSetterMethodName("get",
                    ((ICobolArrayComplexBinding) binding).
                    getComplexItemBinding());
        }
        return getterSetterMethodName("get", binding);
    }

    /**
     * Builds a set method name for a field name.
     * @param binding a bound element
     * @return a getter method
     */
    public final String setterMethodName(final ICobolBinding binding) {
        /* Jaxb objects export lists rather than wrappers */
        if (binding instanceof ICobolArrayComplexBinding) {
            return getterSetterMethodName("set",
                    ((ICobolArrayComplexBinding) binding).
                    getComplexItemBinding());
        }
        return getterSetterMethodName("set", binding);
    }

    /**
     * Creates get/set method name.
     * @param prefix either get or set
     * @param binding the element
     * @return a method name to either get or set this element
     */
    private String getterSetterMethodName(
            final String prefix, final ICobolBinding binding) {
        String fieldName = JaxbUtil.getFieldName(binding);
        if (fieldName == null || fieldName.length() == 0) {
            throw new IllegalArgumentException(fieldName);
        }
        if (fieldName.length() == 1) {
            return prefix + fieldName.toUpperCase(Locale.getDefault());
        }
        return prefix
        + fieldName.substring(0, 1).toUpperCase(Locale.getDefault())
        + fieldName.substring(1, fieldName.length());
    }

    /**
     * Evaluates if element is an array.
     * @param binding the element
     * @return true if its an array
     */
    public final boolean isArray(final ICobolBinding binding) {
        if (getGenericType(binding).equals("complexArray")) {
            return true;
        }
        /* JAXB considers a member to be an array only if maxOccurs > 1*/
        if (binding.getMaxOccurs() > 1) {
            return true;
        }
        return false;
    }

    /**
     * Evaluates if element is a variable size array.
     * @param binding a bound element
     * @return true if variable size array
     */
    public final boolean isVariableSizeArray(final ICobolBinding binding) {
        return (binding.getMaxOccurs() > 1
                && binding.getMinOccurs() < binding.getMaxOccurs());
    }

    /**
     * Evaluates if element is optional.
     * <p/>
     * COBOL data items declared with OCCURS 0 TO 1 DEPENDING ON are not
     * arrays from a JAXB standpoint. We call them optional elements.
     * @param binding a bound element
     * @return true if element is optional. i.e. its existence depends on
     *  a counter value being 1.
     */
    public final boolean isOptional(final ICobolBinding binding) {
        return (binding.getDependingOn() != null
                && binding.getDependingOn().length() > 0);
    }

    /**
     * A mere wrapper on the static <code>JaxbUtil.byteLength</code>.
     * TODO revise JaxbUtil to get a numeric rather than a string
     * @param jaxbPackage the JAXB package name from which an ObjectFactory
     *        can be instanciated
     * @param jaxbTypeName the JAXB type name of the object for which byte 
     *        length must be returned
     * @return the byte length as a string
     * @throws HostException if byte length calculation failed
     */
    public final long getByteLength(
            final String jaxbPackage,
            final String jaxbTypeName) throws HostException {
        return Long.parseLong(JaxbUtil.byteLength(jaxbPackage, jaxbTypeName));
    }

    /**
     * A mere wrapper on the static <code>JaxbUtil.getJavaClassName</code>.
     * @param jaxbPackage the JAXB package name from which a java class name
     *        is to be derived
     * @param jaxbTypeName the JAXB type name from which a java class name
     *        is to be derived
     * @return a class name (including package name) that the JAXB class
     * is hiding or the JAXB class itself if it is not hiding a POJO.
     * @throws HostException if deriving a java class name fails
     */
    public final String getJavaClassName(
            final String jaxbPackage,
            final String jaxbTypeName) throws HostException {
        return JaxbUtil.getJavaClassName(jaxbPackage, jaxbTypeName);
    }

}
