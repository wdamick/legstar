package com.legstar.test.cixs.lsfilean;

import javax.xml.bind.annotation.XmlRegistry;
import com.legstar.test.cixs.lsfilean.oper.LsfileaeFaultInfo;
import com.legstar.test.cixs.lsfilean.oper.LsfileaeRequest;
import com.legstar.test.cixs.lsfilean.oper.LsfileaeResponse;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.lsfilean.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link LsfileanHostHeader }
     * 
     */
    public LsfileanHostHeader createLsfileanHostHeader() {
        return new LsfileanHostHeader();
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

}
