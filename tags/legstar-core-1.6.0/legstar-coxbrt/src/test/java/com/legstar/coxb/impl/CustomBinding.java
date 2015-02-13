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
package com.legstar.coxb.impl;

import java.sql.Timestamp;
import java.util.Date;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;

/**
 * An example of a custom binding.
 * <p/>
 * Used via an annotation: @CobolJavaTypeAdapter(value = CustomBinding.class)
 * <p/>
 * Shows how a java java.sql.Timestamp can be used in place of a java
 * java.util.Date.
 * 
 */
public class CustomBinding extends CStringBinding {

    public CustomBinding(String bindingName, String jaxbName,
            Class < ? > jaxbType, CobolElement cobolAnnotations,
            ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
    }

    /** {@inheritDoc} */
    public void setObjectValue(final Object value) throws HostException {
        if (value instanceof Timestamp) {
            super.setObjectValue(new Date(((Timestamp) value).getTime()));
        } else {
            super.setObjectValue(value);
        }
    }

    /** {@inheritDoc} */
    public Object getObjectValue(final Class < ? > type) throws HostException {

        if (type.equals(Timestamp.class)) {
            return new Timestamp(
                    ((Date) super.getObjectValue(Date.class)).getTime());
        } else {
            return super.getObjectValue(type);
        }
    }
}
