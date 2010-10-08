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
package com.legstar.coxb.cust.redopera;
import java.util.Hashtable;

import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;
import com.legstar.test.coxb.redopera.Dfhcommarea;

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

        /* Get the current value of the function variable. */
        Dfhcommarea jobj = (Dfhcommarea) choice.getParentValueObject();

        if (jobj.getCFunction().trim().compareTo("stringMethod") == 0) {
            return choice.getAlternativeByName("Filler25");
        }

        if (jobj.getCFunction().trim().compareTo("intMethod") == 0) {
            return choice.getAlternativeByName("Filler28");
        }

        /* None of the alternatives could be chosen. Raise an error.*/
        throw (new HostException("Unrecognized CFunction value:" 
                + jobj.getCFunction()));
    }

}

