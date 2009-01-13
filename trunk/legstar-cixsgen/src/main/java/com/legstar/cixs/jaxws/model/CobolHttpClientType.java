package com.legstar.cixs.jaxws.model;

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
