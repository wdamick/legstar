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
package com.legstar.config;

/**
 * Configuration-related constant values.
 */
public class Constants {

    /** Property identifier for the host program name. */
    public static final String CICS_PROGRAM_NAME_KEY = "CICSProgramName";

    /** Property identifier for program commarea length. */
    public static final String CICS_LENGTH_KEY = "CICSLength";

    /** Property identifier for program input data length. */
    public static final String CICS_DATALEN_KEY = "CICSDataLength";

    /** Property identifier for program system ID. */
    public static final String CICS_SYSID_KEY = "CICSSysID";

    /** Property identifier for program sync on return parm. */
    public static final String CICS_SYNCONRET_KEY = "CICSSyncOnReturn";

    /** Property identifier for program transaction ID. */
    public static final String CICS_TRANSID_KEY = "CICSTransID";

    /** Property identifier for channel ID. */
    public static final String CICS_CHANNEL_KEY = "CICSChannel";

    /** Property identifier for input containers array. */
    public static final String CICS_IN_CONTAINERS_KEY = "CICSInContainers";

    /** Property identifier for an input container max size (host bytes). */
    public static final String CICS_IN_CONTAINERS_LEN_KEY =
        "CICSInContainersLength";

    /** Property identifier for output containers array. */
    public static final String CICS_OUT_CONTAINERS_KEY = "CICSOutContainers";

    /** Property identifier for an output container max size (host bytes). */
    public static final String CICS_OUT_CONTAINERS_LEN_KEY =
        "CICSOutContainersLength";

    /** Utility class. */
    private Constants() {
        
    }

}
