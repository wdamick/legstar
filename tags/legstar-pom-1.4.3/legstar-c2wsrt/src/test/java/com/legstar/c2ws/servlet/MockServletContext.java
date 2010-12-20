/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.c2ws.servlet;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Set;

import javax.servlet.RequestDispatcher;
import javax.servlet.Servlet;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;

/**
 * Simple Mock for ServletContext.
 */
public class MockServletContext implements ServletContext {

    /** Inner set of attributes.*/
    private final Hashtable < String , Object >  mAttributes
        = new Hashtable < String, Object >();

    /** {@inheritDoc} */
    public Object getAttribute(final String name) {
        return mAttributes.get(name);
    }

    /** {@inheritDoc} */
    public Enumeration < ? > getAttributeNames() {
        return mAttributes.keys();
    }

    /** {@inheritDoc} */
    public ServletContext getContext(final String uripath) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public String getInitParameter(final String name) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public Enumeration < ? > getInitParameterNames() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public int getMajorVersion() {
        // TODO Auto-generated method stub
        return 0;
    }

    /** {@inheritDoc} */
    public String getMimeType(final String file) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public int getMinorVersion() {
        // TODO Auto-generated method stub
        return 0;
    }

    /** {@inheritDoc} */
    public RequestDispatcher getNamedDispatcher(final String name) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public String getRealPath(final String path) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public RequestDispatcher getRequestDispatcher(final String path) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public URL getResource(final String path) throws MalformedURLException {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public InputStream getResourceAsStream(final String path) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public Set < ? > getResourcePaths(final String arg0) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public String getServerInfo() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public Servlet getServlet(final String name) throws ServletException {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public String getServletContextName() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public Enumeration < ? > getServletNames() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public Enumeration < ? > getServlets() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    public void log(final String msg) {
        // TODO Auto-generated method stub

    }

    /** {@inheritDoc} */
    public void log(final Exception exception, final String msg) {
        // TODO Auto-generated method stub

    }

    /** {@inheritDoc} */
    public void log(final String message, final Throwable throwable) {
        // TODO Auto-generated method stub

    }

    /** {@inheritDoc} */
    public void removeAttribute(final String name) {
        // TODO Auto-generated method stub

    }

    /** {@inheritDoc} */
    public void setAttribute(final String name, final Object value) {
        if (value != null) {
            mAttributes.put(name, value);
        } else {
            mAttributes.remove(name);
        }
    }

    /** {@inheritDoc} */
    public String getContextPath() {
        return null;
    }



}
