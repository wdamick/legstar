package com.legstar.test.cixs.lsfileaq;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfileaq.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link LsfileaqHostHeader }
     * 
     */
    public LsfileaqHostHeader createLsfileaqHostHeader() {
        return new LsfileaqHostHeader();
    }

    /**
     * @return an instance of {@link LsfileaqRequest }
     * 
     */
    public LsfileaqRequest createLsfileaqRequest() {
        return new LsfileaqRequest();
    }

    /**
     * @return an instance of {@link LsfileaqResponse }
     * 
     */
    public LsfileaqResponse createLsfileaqResponse() {
        return new LsfileaqResponse();
    }


    /**
     * @return an instance of {@link LsfileaqFaultInfo }
     * 
     */
    public LsfileaqFaultInfo createLsfileaqFaultInfo() {
        return new LsfileaqFaultInfo();
    }

}
