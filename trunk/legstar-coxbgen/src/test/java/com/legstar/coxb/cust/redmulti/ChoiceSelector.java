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

package com.legstar.coxb.cust.redmulti;
import java.util.Hashtable;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.host.HostException;
import com.legstar.test.coxb.redmulti.DfhcommareaType;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

  /* (non-Javadoc)
   * @see com.legstar.coxb.ICobolUnmarshalChoiceStrategy#choose(com.legstar.coxb.ICobolChoiceBinding, java.util.Hashtable, com.legstar.coxb.CobolElementVisitor)
   */
  public final ICobolBinding choose(
      final ICobolChoiceBinding choice,
      final Hashtable < String, Object > variablesMap,
      CobolElementVisitor visitor)
    throws HostException {
    
		/* Get the current value of the cOutputType variable. */
		DfhcommareaType jobj = (DfhcommareaType) choice.getValue();
		
		if (jobj.getCOutputType().compareTo("normal") == 0) {
			return choice.getAlternativeByJavaName("filler35");
		}

		if (jobj.getCOutputType().compareTo("error ") == 0) {
			return choice.getAlternativeByJavaName("filler38");
		}

		/* None of the alternatives could be chosen. Raise an error.*/
		throw (new HostException("Unrecognized cOutputType value:" 
				+ jobj.getCOutputType()));
  }

}
  
