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
  
