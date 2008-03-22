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
package com.legstar.xsdc.test.cases.jvmquery;

import java.util.ArrayList;
import java.util.List;

/**
 * Caller sends this type of request by passing a list of environment variable
 * names which values must be retrieved.
  */
public class JVMQueryRequest {
    
    private List <String> mEnvVarNames = new ArrayList <String>();

    /**
     * @return the environment variable names to get
     */
    public final List <String> getEnvVarNames() {
        return mEnvVarNames;
    }

    /**
     * @param envVarNames the the environment variable names to set
     */
    public final void setEnvVarNames(List<String> envVarNames) {
        mEnvVarNames = envVarNames;
    }

}
