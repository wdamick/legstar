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
package com.legstar.coxb.cust.redsimpt;
import java.util.Hashtable;

import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/** 
 * An example of a choice strategy based on numericity. If content is numeric
 * the numeric alternative is chosen.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

    /** List of digit characters. */
    private static byte[] hostChars =
        HostData.toByteArray("F0F1F2F3F4F5F6F7F8F9");

    /** {@inheritDoc} */
    public ICobolBinding choose(
            final ICobolChoiceBinding choice,
            final Hashtable < String, Object > variablesMap,
            final CobolElementVisitor visitor)
    throws HostException {

        /* Examine the nature of the host data. If any character is not a digit,
         * select the string alternative.
         * In this special case, both alternatives have the same size, so we can
         * safely use calcByteLength to determine that size.  */
        boolean digits = false;
        for (int i = 0; i < choice.calcByteLength(); i++) {
            digits = false;
            for (int j = 0; j < hostChars.length; j++) {
                if (visitor.getHostBytes()[visitor.getOffset() + i]
                                           == hostChars[j]) {
                    digits = true;
                    break;
                }
            }
            if (!digits) {
                break;
            }
        }

        /* If all characters are digits, we can safely select the numeric
         * choice. Otherwise select the alphabetic alternative. */
        if (digits) {
            return choice.getAlternativeByName("CDefinition2");
        } else {
            return choice.getAlternativeByName("CDefinition1");
        }
    }

}

