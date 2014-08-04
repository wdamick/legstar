package com.legstar.test.cixs.lsfileam;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfileam.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link LsfileamHostHeader }
     * 
     */
    public LsfileamHostHeader createLsfileamHostHeader() {
        return new LsfileamHostHeader();
    }

    /**
     * @return an instance of {@link LsfileamRequest }
     * 
     */
    public LsfileamRequest createLsfileamRequest() {
        return new LsfileamRequest();
    }

    /**
     * @return an instance of {@link LsfileamResponse }
     * 
     */
    public LsfileamResponse createLsfileamResponse() {
        return new LsfileamResponse();
    }

    /**
     * @return an instance of {@link LsfileamRequestHolder }
     * 
     */
    public LsfileamRequestHolder createLsfileamRequestHolder() {
        return new LsfileamRequestHolder();
    }

    /**
     * @return an instance of {@link LsfileamResponseHolder }
     * 
     */
    public LsfileamResponseHolder createLsfileamResponseHolder() {
        return new LsfileamResponseHolder();
    }

    /**
     * @return an instance of {@link LsfileamFaultInfo }
     * 
     */
    public LsfileamFaultInfo createLsfileamFaultInfo() {
        return new LsfileamFaultInfo();
    }

}
