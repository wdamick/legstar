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

package com.legstar.coxb.cust.redsimpt;
import java.util.Hashtable;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {

	/** List of digit characters */
	private static byte[] hostChars =
		HostData.toByteArray("F0F1F2F3F4F5F6F7F8F9");

  /* (non-Javadoc)
   * @see com.legstar.coxb.ICobolUnmarshalChoiceStrategy#choose(com.legstar.coxb.ICobolChoiceBinding, java.util.Hashtable, com.legstar.coxb.CobolElementVisitor)
   */
  public final ICobolBinding choose(
      final ICobolChoiceBinding choice,
      final Hashtable < String, Object > variablesMap,
      CobolElementVisitor visitor)
    throws HostException {
    
		/* Examine the nature of the host data. If any character is not a digit,
		 * select the string alternative.  */
		boolean digits = false;
		for (int i = 0; i < choice.getByteLength(); i ++) {
			digits = false;
			for (int j = 0; j < hostChars.length; j ++) {
				if (visitor.getHostBytes()[visitor.getOffset() + i] == hostChars[j]) {
					digits = true;
					break;
				}
			}
		}
		
		/* If all characters are digits, we can safely select the numeric choice.
		 * otherwise select the alphabetic alternative. */
		if (digits) {
			return choice.getAlternativeByJavaName("cDefinition2");
		} else {
			return choice.getAlternativeByJavaName("cDefinition1");
		}
  }

}
  
