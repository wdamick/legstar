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
package com.legstar.xsdc.gen;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.xml.namespace.NamespaceContext;

/** Helper class needed by XsdCobolAnnotatorTest */
public class NamespaceContextImpl implements NamespaceContext{
	
	private Map < String, String > mNamespaceMap = new HashMap < String, String >();
	
	public NamespaceContextImpl(){}
	
	public String getPrefix(String uri){
		for (String prefix : mNamespaceMap.keySet()) {
			if (mNamespaceMap.get(prefix).compareTo(uri) == 0) {
				return prefix;
			}
		}
		 return null;
	}
	 
	 public void addNamespace(String prefix, String uri){
		mNamespaceMap.put(prefix, uri);
	 }
	 
	 public Iterator < String > getPrefixes(String uri){
		 return mNamespaceMap.keySet().iterator();
	 }
	 
	 public String getNamespaceURI(String prefix){
		 return mNamespaceMap.get(prefix);
	 }
}
