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
package com.legstar.proxy.invoke;

/**
 * Error with the target web services configurations.
 */
public class ProxyConfigurationException extends Exception {

    /** Default serial ID. */
    private static final long serialVersionUID = -5413324572101279802L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public ProxyConfigurationException(final String message) {
        super(message);
    }
    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public ProxyConfigurationException(final Exception e) {
        super(e);
    }
    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public ProxyConfigurationException(final Throwable e) {
        super(e);
    }

}
