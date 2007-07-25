
package com.legstar.c2ws.sample.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "getInfoResponse", namespace = "http://sample.c2ws.legstar.com/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "getInfoResponse", namespace = "http://sample.c2ws.legstar.com/")
public class GetInfoResponse {

    @XmlElement(name = "return", namespace = "")
    private com.legstar.c2ws.sample.CultureInfoReply _return;

    /**
     * 
     * @return
     *     returns CultureInfoReply
     */
    public com.legstar.c2ws.sample.CultureInfoReply getReturn() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void setReturn(com.legstar.c2ws.sample.CultureInfoReply _return) {
        this._return = _return;
    }

}
