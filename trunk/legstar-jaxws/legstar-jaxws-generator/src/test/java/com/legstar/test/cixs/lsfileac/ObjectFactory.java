package com.legstar.test.cixs.lsfileac;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfileac.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link LsfileacHostHeader }
     * 
     */
    public LsfileacHostHeader createLsfileacHostHeader() {
        return new LsfileacHostHeader();
    }

    /**
     * @return an instance of {@link LsfileacRequest }
     * 
     */
    public LsfileacRequest createLsfileacRequest() {
        return new LsfileacRequest();
    }

    /**
     * @return an instance of {@link LsfileacResponse }
     * 
     */
    public LsfileacResponse createLsfileacResponse() {
        return new LsfileacResponse();
    }

    /**
     * @return an instance of {@link LsfileacRequestHolder }
     * 
     */
    public LsfileacRequestHolder createLsfileacRequestHolder() {
        return new LsfileacRequestHolder();
    }

    /**
     * @return an instance of {@link LsfileacResponseHolder }
     * 
     */
    public LsfileacResponseHolder createLsfileacResponseHolder() {
        return new LsfileacResponseHolder();
    }

    /**
     * @return an instance of {@link LsfileacFaultInfo }
     * 
     */
    public LsfileacFaultInfo createLsfileacFaultInfo() {
        return new LsfileacFaultInfo();
    }

}
