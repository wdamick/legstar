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
package com.legstar.cixs.gen.model.options;

/**
 * Wire protocol used between a client and an adapter, an adapter and the host,
 * the host and a proxy, etc...
 */
public enum TransportType {
    /** Sockets.*/
    SOCKETS,
    /** HTTP. */
    HTTP,
    /** Websphere MQ. */
    WMQ
}
