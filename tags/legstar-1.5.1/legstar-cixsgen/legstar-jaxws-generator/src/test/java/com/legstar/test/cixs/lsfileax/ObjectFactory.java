package com.legstar.test.cixs.lsfileax;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfileax.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link LsfileaxHostHeader }
     * 
     */
    public LsfileaxHostHeader createLsfileaxHostHeader() {
        return new LsfileaxHostHeader();
    }

    /**
     * @return an instance of {@link LsfileaeRequest }
     * 
     */
    public LsfileaeRequest createLsfileaeRequest() {
        return new LsfileaeRequest();
    }

    /**
     * @return an instance of {@link LsfileaeResponse }
     * 
     */
    public LsfileaeResponse createLsfileaeResponse() {
        return new LsfileaeResponse();
    }


    /**
     * @return an instance of {@link LsfileaeFaultInfo }
     * 
     */
    public LsfileaeFaultInfo createLsfileaeFaultInfo() {
        return new LsfileaeFaultInfo();
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
