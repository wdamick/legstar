/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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

/** 
 * Helper class needed by XsdCobolAnnotatorTest.
 *  */
public class NamespaceContextImpl implements NamespaceContext {

    /**
     * A namespace map.
     */
    private Map < String, String > mNamespaceMap = new HashMap < String, String >();

    /** An empty constructor. */
    public NamespaceContextImpl() { };

    /** {@inheritDoc} */
    public String getPrefix(final String uri) {
        for (String prefix : mNamespaceMap.keySet()) {
            if (mNamespaceMap.get(prefix).compareTo(uri) == 0) {
                return prefix;
            }
        }
        return null;
    }

    /**
     * Add a namespace.
     * @param prefix namespace prefix
     * @param uri namespace URI
     */
    public void addNamespace(final String prefix, final String uri) {
        mNamespaceMap.put(prefix, uri);
    }

    /** {@inheritDoc} */
    public Iterator < String > getPrefixes(final String uri) {
        return mNamespaceMap.keySet().iterator();
    }

    /** {@inheritDoc} */
    public String getNamespaceURI(final String prefix) {
        return mNamespaceMap.get(prefix);
    }
}
