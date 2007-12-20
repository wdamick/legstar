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
	public final ICobolBinding choose(
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

