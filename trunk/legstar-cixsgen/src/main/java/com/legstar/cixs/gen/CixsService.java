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
package com.legstar.cixs.gen;

import java.util.List;
import java.util.ArrayList;

/**
 * This class describes a service.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsService {
	
	/** Service name. */
	private String mServiceName;

	/** Service endpoint package name. */
	private String mEndpointPackageName;
	
	/** Service target namespace. */
	private String mTargetNamespace;
	
	/** The service list of operations. */
	private List < CixsOperation > mOperations =
		new ArrayList < CixsOperation >();
	
	/**
	 * @return the service name
	 */
	public final String getServiceName() {
		return mServiceName;
	}

	/**
	 * @param serviceName the service name to set
	 */
	public final void setServiceName(final String serviceName) {
		mServiceName = serviceName;
	}

	/**
	 * @return the service endpoint package name
	 */
	public final String getEndpointPackageName() {
		return mEndpointPackageName;
	}

	/**
	 * @param endpointPackageName the service endpoint package name to set
	 */
	public final void setEndpointPackageName(final String endpointPackageName) {
		mEndpointPackageName = endpointPackageName;
	}

	/**
	 * @return the service target namespace
	 */
	public final String getTargetNamespace() {
		return mTargetNamespace;
	}

	/**
	 * @param targetNamespace the service target namespace to set
	 */
	public final void setTargetNamespace(final String targetNamespace) {
		mTargetNamespace = targetNamespace;
	}

	/**
	 * @return the service list of operations
	 */
	public final List < CixsOperation > getOperations() {
		return mOperations;
	}

	/**
	 * @param operations the list of operations to set
	 */
	public final void setOperations(
			final List < CixsOperation > operations) {
		mOperations = operations;
	}

	/**
	 * @param operation the operation to add
	 */
	public final void addOperation(final CixsOperation operation) {
		mOperations.add(operation);
	}
	
	/**
	 * Create an XML usable as input for and ant task.
	 * @return the XML
	 */
	public final String serialize() {
		StringBuffer result = new StringBuffer();
		result.append("<service serviceName="
				+ '\"' + mServiceName + '\"');
		result.append(" endpointPackageName="
				+ '\"' + mEndpointPackageName + '\"');
		result.append(" targetNamespace="
				+ '\"' + mTargetNamespace + '\"');
		result.append('>');
		for (int i = 0; i < mOperations.size(); i++) {
			CixsOperation op = mOperations.get(i);
			result.append("<operation operationName=" + '\"'
					+ op.getOperationName() + '\"');
			result.append(" programName=" + '\"'
					+ op.getProgramName() + '\"');
			result.append(" inputJaxbType=" + '\"'
					+ op.getInputJaxbType() + '\"');
			result.append(" inputJaxbPackageName="	+ '\"'
					+ op.getInputJaxbPackageName() + '\"');
			result.append(" outputJaxbType=" + '\"'
					+ op.getOutputJaxbType() + '\"');
			result.append(" outputJaxbPackageName="	+ '\"'
					+ op.getOutputJaxbPackageName() + '\"');
			result.append("/>");
		}
		result.append("</service>");
		return result.toString();
	}

}
