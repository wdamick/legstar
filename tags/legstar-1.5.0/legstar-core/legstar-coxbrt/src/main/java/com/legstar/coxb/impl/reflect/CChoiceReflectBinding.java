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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.common.CChoiceBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.util.ClassUtil;

/**
 * Represents a choice between 2 or more elements. A choice results from a cobol
 * REDEFINES clause exposed as an xs:choice in the corresponding XML schema
 * 
 * @author Fady Moussallam
 * 
 */
public class CChoiceReflectBinding extends CChoiceBinding {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * A choice element gets created when a redefined item is encountered. The
     * constructor gets invoked with the redefined item characteristics.
     * 
     * @param bindingName the identifier for this binding
     * @param cobolAnnotations the cobol annotations for the first alternative
     * @param parentBinding a reference to the parent binding if any
     */
    public CChoiceReflectBinding(final String bindingName,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {

        super(bindingName, cobolAnnotations, parentBinding);
    }

    /** {@inheritDoc} */
    public void setAlternativesValues() throws HostException {
        for (ICobolBinding alt : getAlternativesList()) {
            /*
             * Choice children are a special case. They directly set their
             * parent object depending on the chosen choice strategy.
             */
            if (alt instanceof ICobolChoiceBinding) {
                continue;
            } else {
                Object value = ClassUtil.invokeGetProperty(
                        getParentJaxbObject(), alt.getJaxbName(),
                        alt.getJaxbType(), alt.getMaxOccurs());
                if (value != null) {
                    if (_log.isDebugEnabled()) {
                        _log.debug("Getting value from JAXB property "
                                + alt.getJaxbName() + " value=" + value);
                    }
                    alt.setObjectValue(value);
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
        /* Set the JAXB object property value from binding object */
        ICobolBinding alt = getAlternativesList().get(index);

        /*
         * Choice children are a special case. They directly set their parent
         * object depending on the chosen choice strategy.
         */
        if (alt instanceof ICobolChoiceBinding) {
            return;
        }

        Object value = alt.getObjectValue(alt.getJaxbType());
        if (_log.isDebugEnabled()) {
            _log.debug("Setting value of JAXB property " + alt.getJaxbName()
                    + " value=" + value);
        }
        ClassUtil.invokeSetProperty(getParentJaxbObject(), alt.getJaxbName(),
                value, alt.getJaxbType());
    }

    /** {@inheritDoc} */
    public Object getParentJaxbObject() throws HostException {
        return getParentBinding().getObjectValue(
                getParentBinding().getJaxbType());
    }

    /** {@inheritDoc} */
    public Object getParentValueObject() throws HostException {
        return getParentJaxbObject();
    }

    /** {@inheritDoc} */
    public Object getObjectValue(final Class < ? > type) throws HostException {
        throw new HostException("Attempt to get value from choice binding "
                + getCobolName());
    }

    /** {@inheritDoc} */
    public void setObjectValue(final Object value) throws HostException {
        throw new HostException("Attempt to set value for choice binding "
                + getCobolName());
    }

    /** {@inheritDoc} */
    public boolean isSet() {
        /*
         * A Choice is considered set if at least one of its alternatives is
         * set.
         */
        for (ICobolBinding alt : getAlternativesList()) {
            if (alt.isSet()) {
                return true;
            }
        }
        return false;
    }
}
