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
package com.legstar.messaging;

import org.apache.commons.configuration.HierarchicalConfiguration;

/** @deprecated */
public class Address extends LegStarAddress {

    /**
     * @param endPointName the target endpoint name
     */
    public Address(final String endPointName) {
        super(endPointName);
    }

    /**
     * @param address the partial address
     * @param endpointConfig an XML configuration fragment
     */
    public Address(final LegStarAddress address,
            final HierarchicalConfiguration endpointConfig) {
        super(address, endpointConfig);
    }

    /**
     * @param endpointConfig an XML configuration fragment
     */
    public Address(final HierarchicalConfiguration endpointConfig) {
        super(endpointConfig);
    }

}
