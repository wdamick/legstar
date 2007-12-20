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
package com.legstar.xslt;

/**
 * This class is used to pass parameters to an XSLT transform. These parameters
 * can be exploited by the style sheet.
 * 
 * @author Fady Moussallam
 * 
 */
public class XSLTParameter {
	
    /** No-arg constructor. */
	public XSLTParameter() {
	}

    /** Parameter name. */
	private String name;
	
	/** Parameter value. */
    private String expression;
    
    /**
     * @param data name to set parameter
     */
    public final void setName(final String data) {
    	this.name = data;
    }
    /**
     * @return parameter name
     */
    public final String getName() { return name; }

    /**
     * @param data expression to set parameter
     */
    public final void setExpression(final String data) {
    	this.expression = data;
    }
    /**
     * @return parameter expression
     */
    public final String getExpression() { return expression; }

}
