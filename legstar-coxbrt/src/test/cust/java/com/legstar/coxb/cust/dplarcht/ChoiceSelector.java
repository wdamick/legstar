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
package com.legstar.coxb.cust.dplarcht;
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
    public final ICobolBinding choose(
        final ICobolChoiceBinding choice,
        final Hashtable < String, Object > variablesMap,
        final CobolElementVisitor visitor) throws HostException {
    
		/* Because the java class hierarchy is complex, we use a mechanism that
		 * allows elements annotated as customVariable to have their value
		 * stored in a simple hashtable for quick access. In this case, the
		 * element marked as customVariable is numeric.
		 * */
	    Integer value =
			(Integer) variablesMap.get("LsRequestType");
		if (value == null) {
			throw (new HostException("Custom variable LsRequestType not set."));
		}
		
		switch (value) {
		case 0:
			return choice.getAlternativeByName("LsFilesData");
		case 1:
			return choice.getAlternativeByName("LsProgramsData");
		case 2:
			return choice.getAlternativeByName("LsTransactionsData");
		default:
			/* None of the alternatives could be chosen, return null to let the
			 * default behavior take over.*/
			return null;
		}
		
    }

}
  
