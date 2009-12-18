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
package com.legstar.coxb;

import com.legstar.coxb.host.HostException;

import java.util.Hashtable;

/**
 * This interface is used for custom choice selection at unmarshaling time.
 * Unmarshaling means host data needs to be converted to Java types.
 * When multiple alternatives are available (REDEFINES), custom code can provide
 * the logic to determine which alternative should be chosen.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolUnmarshalChoiceStrategy {
    /**
     * Determines which alternative should be chosen.
     * @param choice the redefined element
     * @param variablesMap variables which values might help with the choice
     * @param visitor the current visitor
     * @return the alternative chosen
     * @throws HostException if error found while looking for alternatives
     */
    ICobolBinding choose(
            ICobolChoiceBinding choice,
            Hashtable < String, Object > variablesMap,
            CobolElementVisitor visitor) throws HostException;

}
