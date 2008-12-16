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
package com.legstar.coxb.convert;

import com.legstar.coxb.CobolContext;

/**
 * This interface is implemented by converters, each specializing in a COBOL data type.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolConverter {

    /**
     * @return Returns the CobolContext.
     */
    CobolContext getCobolContext();

    /**
     * @param cobolContext The CobolContext to set.
     */
    void setCobolContext(final CobolContext cobolContext);

    
 }
