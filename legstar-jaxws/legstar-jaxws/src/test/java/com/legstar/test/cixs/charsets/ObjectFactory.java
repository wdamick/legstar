package com.legstar.test.cixs.charsets;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * A JAXB ObjectFactory for wrapper classes.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes
     * for package: com.legstar.test.cixs.charsets.
     * 
     */
    public ObjectFactory() {
    }

    /**
     * @return an instance of {@link CharsetsHostHeader }
     * 
     */
    public CharsetsHostHeader createCharsetsHostHeader() {
        return new CharsetsHostHeader();
    }

    /**
     * @return an instance of {@link CharsetsRequest }
     * 
     */
    public CharsetsRequest createCharsetsRequest() {
        return new CharsetsRequest();
    }

    /**
     * @return an instance of {@link CharsetsResponse }
     * 
     */
    public CharsetsResponse createCharsetsResponse() {
        return new CharsetsResponse();
    }


    /**
     * @return an instance of {@link CharsetsFaultInfo }
     * 
     */
    public CharsetsFaultInfo createCharsetsFaultInfo() {
        return new CharsetsFaultInfo();
    }

}
