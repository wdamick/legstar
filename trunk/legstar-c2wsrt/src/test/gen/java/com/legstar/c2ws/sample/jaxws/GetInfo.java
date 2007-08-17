
package com.legstar.c2ws.sample.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "getInfo", namespace = "http://sample.c2ws.legstar.com/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "getInfo", namespace = "http://sample.c2ws.legstar.com/")
public class GetInfo {

    @XmlElement(name = "arg0", namespace = "")
    private com.legstar.c2ws.sample.CultureInfoRequest arg0;

    /**
     * 
     * @return
     *     returns CultureInfoRequest
     */
    public com.legstar.c2ws.sample.CultureInfoRequest getArg0() {
        return this.arg0;
    }

    /**
     * 
     * @param arg0
     *     the value for the arg0 property
     */
    public void setArg0(com.legstar.c2ws.sample.CultureInfoRequest arg0) {
        this.arg0 = arg0;
    }

}
