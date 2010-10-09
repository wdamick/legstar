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
package com.legstar.coxb.cust.redbotha;
import java.util.Hashtable;

import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public ICobolBinding choose(
            final ICobolChoiceBinding choice,
            final Hashtable < String, Object > variablesMap,
            final CobolElementVisitor visitor)
    throws HostException {

        /* Save the current offset in the visitor */
        int saveOffset = visitor.getOffset();

        /* Force visitor to visit the second alternative (the default
         * code will process the first one) and update the JAXB object*/
        choice.getAlternativesList().get(1).accept(visitor);
        choice.setPropertyValue(1);

        /* Now restore offset and pretend no alternative was processed.*/
        visitor.setOffset(saveOffset);
        return null;
    }

}

