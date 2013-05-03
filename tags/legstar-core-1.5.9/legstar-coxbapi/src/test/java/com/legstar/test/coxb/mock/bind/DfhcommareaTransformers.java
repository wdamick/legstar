/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb.mock.bind;

import com.legstar.coxb.transform.AbstractTransformers;
import com.legstar.coxb.transform.HostTransformException;

/**
 * A mock transformers class.
 * 
 */
public class DfhcommareaTransformers extends AbstractTransformers {

    /** {@inheritDoc} */
    public Object toJava(final byte[] hostData, final String hostCharset)
            throws HostTransformException {
        return null;
    }

    /** {@inheritDoc} */
    public Object toJava(final byte[] hostData) throws HostTransformException {
        return null;
    }

}
