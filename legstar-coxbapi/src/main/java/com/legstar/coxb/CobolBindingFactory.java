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

/**
 * Instanciate a concrete binding factory.
 */
public final class CobolBindingFactory {
	
	/** In this version the factory name is hardcoded. In a future release,
	 * this will be pulled from some configuration file. */
	private static final String FACTORY_NAME =
		"com.legstar.coxb.impl.CBindingFactory";
	
	/** Private constructor to prevent instanciation of this final class. */
	private CobolBindingFactory() {
	}
	
	/**
	 * Create a concrete binding factory.
	 * @return a binding factory ready to create binding elements
	 */
	public static ICobolBindingFactory getBindingFactory() {
		try {
			Class < ? > ofClass = Class.forName(FACTORY_NAME);
			Object of = ofClass.newInstance();
			return (ICobolBindingFactory) of;
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

}
