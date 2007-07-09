/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.coxb.impl.reflect;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.common.CChoiceBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.util.JaxbUtil;

/**
 * Represents a choice between 2 or more elements. A choice results from a cobol
 * REDEFINES clause exposed as an xs:choice in the corresponding XML schema
 *
 * @author Fady Moussallam
 * 
*/
public class CChoiceReflectBinding extends CChoiceBinding {

	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(CChoiceReflectBinding.class);
	/**
	 * A choice element gets created when a redefined item is encountered.
	 * The constructor gets invoked with the redefined item characteristics.
	 * 
	 * @param bindingName the identifier for this binding
	 * @param cobolAnnotations the cobol annotations for the first alternative
	 * @param parentBinding a reference to the parent binding if any
	 */
	public CChoiceReflectBinding(
			final String bindingName,
			final CobolElement cobolAnnotations,
			final ICobolComplexBinding parentBinding) {
		
		super(bindingName, cobolAnnotations, parentBinding);
	}

	/** {@inheritDoc} */
	public final void setAlternativesValues() throws HostException {
		for (ICobolBinding alt : getAlternativesList()) {
         	/* Choice children are a special case. They directly set 
         	 * their parent object depending on the chosen choice
         	 * strategy. */
        	if (alt instanceof ICobolChoiceBinding) {
        		continue;
        	} else {
	        	Object value = JaxbUtil.invokeGetProperty(
	        			getParentJaxbObject(), alt.getJaxbName());
	        	if (value != null) {
	        		if (LOG.isDebugEnabled()) {
	        			LOG.debug("Getting value from JAXB property "
	        					+ alt.getJaxbName()
	        					+ " value=" + value);
	        		}
	        		alt.setObjectValue(value);
	        	}
        	}
		}
	}
	
	/** {@inheritDoc} */
	public final void setJaxbPropertyValue(
			final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
    	ICobolBinding alt = getAlternativesList().get(index);

    	/* Choice children are a special case. They directly set 
     	 * their parent object depending on the chosen choice
     	 * strategy. */
    	if (alt instanceof ICobolChoiceBinding) {
    		return;
    	}

		Object value = alt.getObjectValue(alt.getJaxbType());
		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting value of JAXB property "
					+ alt.getJaxbName()
					+ " value=" + value);
		}
		JaxbUtil.invokeSetProperty(getParentJaxbObject(), alt.getJaxbName(),
				value, alt.getJaxbType());
	}
	
    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
		throw new HostException("Attempt to get value from choice binding "
				+ getCobolName());
    }

    /** {@inheritDoc} */
    public final void setObjectValue(final Object value) throws HostException {
		throw new HostException("Attempt to set value for choice binding "
				+ getCobolName());
    }
    
    /** {@inheritDoc} */
	public final boolean isSet() {
		/* A Choice is considered set if at least one of its alternatives
		 * is set. */
		for (ICobolBinding alt : getAlternativesList()) {
			if (alt.isSet()) {
				return true;
			}
		}
		return false;
	}
}
