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

import javax.servlet.ServletException;

import junit.framework.TestCase;

import com.legstar.proxy.invoke.ReflectOperationProxy;
import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;

/**
 * Test the servlet proxy.
 * 
 */
public class C2wsProxyTest extends TestCase {

    /**
     * Instantiate the servlet.
     */
    public void testInstantiate() {
        try {
            MockServletConfig servletConfig = new MockServletConfig();
            servletConfig.addInitParameters(CultureinfoJaxwsCases
                    .getReflectConfig());
            C2wsProxy c2wsProxy = new C2wsProxy();
            c2wsProxy.init(servletConfig);
            assertEquals("GetInfo", c2wsProxy.getServiceProxy().getConfig()
                    .get(ReflectOperationProxy.REQUEST_JAXB_TYPE_PROPERTY));
        } catch (ServletException e) {
            fail(e.getMessage());
        }
    }

}
