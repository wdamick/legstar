/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.cust.redmulti;
import java.util.Hashtable;

import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;
import com.legstar.test.coxb.redmulti.Dfhcommarea;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public final ICobolBinding choose(
            final ICobolChoiceBinding choice,
            final Hashtable < String, Object > variablesMap,
            final CobolElementVisitor visitor)
    throws HostException {

        /* Get the current value of the cOutput variable. */
        Dfhcommarea jobj =
            (Dfhcommarea) choice.getParentValueObject();

        if (jobj.getCOutputType().compareTo("normal") == 0) {
            return choice.getAlternativeByName("Filler35");
        }

        if (jobj.getCOutputType().compareTo("error") == 0) {
            return choice.getAlternativeByName("Filler38");
        }

        /* None of the alternatives could be chosen. Raise an error.*/
        throw (new HostException("Unrecognized COutput value:" 
                + jobj.getCOutputType()));
    }

}

