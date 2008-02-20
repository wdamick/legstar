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
package com.legstar.cixs.gen.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Abstract representation of the mapping between a service and a set of CICS
 * programs.
 *
 */
public abstract class AbstractCixsService {

	/** List of operations provided by this service. */
	private List < CixsOperation > mCixsOperations =
		new ArrayList < CixsOperation >();

	/**
	 * @return the service list of operations
	 */
	public final List<CixsOperation> getCixsOperations() {
		return mCixsOperations;
	}

	/**
	 * @param cixsOperations the service list of operations to set
	 */
	public final void setCixsOperations(final List<CixsOperation> cixsOperations) {
		mCixsOperations = cixsOperations;
	}

    /**
     * Operations are actually a set of uniquely named operations.
     * @param operation the operation to add
     * @throws CixsModelException if operation is a duplicate
     */
    public final void addCixsOperation(
            final CixsOperation operation) throws CixsModelException {
        /* Check that this operation is not already part of the set */
        if (mCixsOperations.contains(operation)) {
            throw new CixsModelException(
                    "This service already contains this operation");
        }
        
        mCixsOperations.add(operation);
    }
    

}
