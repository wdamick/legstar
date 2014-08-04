package com.legstar.test.cixs.lsfileal;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfileal.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link LsfilealHostHeader }
     * 
     */
    public LsfilealHostHeader createLsfilealHostHeader() {
        return new LsfilealHostHeader();
    }

    /**
     * @return an instance of {@link LsfilealRequest }
     * 
     */
    public LsfilealRequest createLsfilealRequest() {
        return new LsfilealRequest();
    }

    /**
     * @return an instance of {@link LsfilealResponse }
     * 
     */
    public LsfilealResponse createLsfilealResponse() {
        return new LsfilealResponse();
    }


    /**
     * @return an instance of {@link LsfilealFaultInfo }
     * 
     */
    public LsfilealFaultInfo createLsfilealFaultInfo() {
        return new LsfilealFaultInfo();
    }

}
