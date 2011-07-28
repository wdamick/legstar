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
 * The types of HTTP sample client desired.
 */
public enum CobolHttpClientType {
    /** Uses LegStar CICS libraries. */
    LSHTTAPI, 
    /** Uses CICS DFHWBCLI. */
    DFHWBCLI,
    /** Uses CICS WEB API. */
    WEBAPI       

}
