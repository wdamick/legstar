package com.legstar.test.cixs.osarrays;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.osarrays.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link OsarraysHostHeader }
     * 
     */
    public OsarraysHostHeader createOsarraysHostHeader() {
        return new OsarraysHostHeader();
    }

    /**
     * @return an instance of {@link OsarraysRequest }
     * 
     */
    public OsarraysRequest createOsarraysRequest() {
        return new OsarraysRequest();
    }

    /**
     * @return an instance of {@link OsarraysResponse }
     * 
     */
    public OsarraysResponse createOsarraysResponse() {
        return new OsarraysResponse();
    }


    /**
     * @return an instance of {@link OsarraysFaultInfo }
     * 
     */
    public OsarraysFaultInfo createOsarraysFaultInfo() {
        return new OsarraysFaultInfo();
    }

}
