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
package com.legstar.coxb.cust.arrayscx;
import java.util.Hashtable;

import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;
//import com.legstar.test.coxb.arrayscx.TableRedefType;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public ICobolBinding choose(
            final ICobolChoiceBinding choice,
            final Hashtable < String, Object > variablesMap,
            final CobolElementVisitor visitor) throws HostException {

        /* Get the parent JAXB object which properties might help select the
         * right alternative. */
        //TableRedefType jaxbo = (TableRedefType) choice.getValue();

        /* Replace following code with actual logic. */
        int index = 0;
        switch (index) {
        case 0:
            return choice.getAlternativeByName("ElementRedef1");
        case 1:
            return choice.getAlternativeByName("ElementRedef2");
        case -1:
            /* An exemple of how to signal an exception.*/
            throw (new HostException("Unable to select an alternative"));
        default:
            /* Null will let the default choice strategy apply. */
            return null;
        }
    }

}

