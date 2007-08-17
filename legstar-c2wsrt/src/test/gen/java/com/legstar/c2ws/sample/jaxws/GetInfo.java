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

package com.legstar.c2ws.sample.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "getInfo", namespace = "http://sample.c2ws.legstar.com/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "getInfo", namespace = "http://sample.c2ws.legstar.com/")
public class GetInfo {

    @XmlElement(name = "arg0", namespace = "")
    private com.legstar.c2ws.sample.CultureInfoRequest arg0;

    /**
     * 
     * @return
     *     returns CultureInfoRequest
     */
    public com.legstar.c2ws.sample.CultureInfoRequest getArg0() {
        return this.arg0;
    }

    /**
     * 
     * @param arg0
     *     the value for the arg0 property
     */
    public void setArg0(com.legstar.c2ws.sample.CultureInfoRequest arg0) {
        this.arg0 = arg0;
    }

}
