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

/**
 * This class describes a mapping between a host structure and a JAXB object.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsStructure {

	/** JAXB complex type. */
	private String mJaxbType;

	/** JAXB package of complex type. */
	private String mJaxbPackageName;

	/** Strategy to resolve input choice alternatives. */
	private String mChoiceStrategy;

	/** A CICS container mapping this structure. */
	private String mContainer;

	/**
	 * @return the JAXB complex type
	 */
	public final String getJaxbType() {
		return mJaxbType;
	}

	/**
	 * @param jaxbType the JAXB complex type to set
	 */
	public final void setJaxbType(final String jaxbType) {
		mJaxbType = jaxbType;
	}

	/**
	 * @return the the JAXB package of complex type
	 */
	public final String getJaxbPackageName() {
		return mJaxbPackageName;
	}

	/**
	 * @param jaxbPackageName the JAXB package of complex type to set
	 */
	public final void setJaxbPackageName(final String jaxbPackageName) {
		mJaxbPackageName = jaxbPackageName;
	}

	/**
	 * @return the strategy to resolve choice alternatives
	 */
	public final String getChoiceStrategy() {
		return mChoiceStrategy;
	}

	/**
	 * @param choiceStrategy the strategy to resolve choice alternatives
	 */
	public final void setChoiceStrategy(final String choiceStrategy) {
		mChoiceStrategy = choiceStrategy;
	}

	/**
	 * @return the CICS container mapping this structure
	 */
	public final String getContainer() {
		return mContainer;
	}

	/**
	 * @param container the CICS container mapping this structure
	 */
	public final void setContainer(final String container) {
		mContainer = container;
	}
}
