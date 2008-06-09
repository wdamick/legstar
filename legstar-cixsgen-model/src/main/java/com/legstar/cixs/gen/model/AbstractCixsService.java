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

import com.legstar.codegen.CodeGenUtil;

/**
 * Abstract representation of the mapping between a service and a set of CICS
 * programs. A Service has a name and some form of Java interface and 
 * implementation.
 *
 */
public abstract class AbstractCixsService extends CixsMappingModel {

	/** Default suffix for class implementation name. */
    private static final String DEFAULT_IMPL_SUFFIX = "Impl";
    
	/** Service package name. */
	private String mPackageName;

	/** Service interface class name. */
	private String mInterfaceClassName;
	
	/** Service implementation class name. */
	private String mImplementationClassName;

	/**
	 * @return the Service package name
	 */
	public final String getPackageName() {
		return mPackageName;
	}

	/**
	 * @param packageName the Service package name to set
	 */
	public final void setPackageName(final String packageName) {
		mPackageName = packageName;
	}

	/**
	 * @return the Service interface class name
	 */
	public final String getInterfaceClassName() {
		if (mInterfaceClassName == null || mInterfaceClassName.length() == 0) {
			return CodeGenUtil.classNormalize(getName());
		}
		return mInterfaceClassName;
	}

	/**
	 * @param interfaceClassName the Service interface class name to set
	 */
	public final void setInterfaceClassName(final String interfaceClassName) {
		mInterfaceClassName = interfaceClassName;
	}

	/**
	 * @return the Service implementation class name
	 */
	public final String getImplementationClassName() {
		if (mImplementationClassName == null 
				|| mImplementationClassName.length() == 0) {
			if (getName() == null) {
				return DEFAULT_IMPL_SUFFIX;
			}
			return CodeGenUtil.classNormalize(getName() + DEFAULT_IMPL_SUFFIX);
		}
		return mImplementationClassName;
	}

	/**
	 * @param implementationClassName the Service implementation class name to
	 *  set
	 */
	public final void setImplementationClassName(
			final String implementationClassName) {
		mImplementationClassName = implementationClassName;
	}

}
