/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
