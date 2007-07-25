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
	 
	 public Iterator getPrefixes(String uri){
		 return mNamespaceMap.keySet().iterator();
	 }
	 
	 public String getNamespaceURI(String prefix){
		 return mNamespaceMap.get(prefix);
	 }
}
