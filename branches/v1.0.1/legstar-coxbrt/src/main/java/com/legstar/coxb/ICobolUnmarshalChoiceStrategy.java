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
package com.legstar.coxb;

import com.legstar.host.HostException;

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
