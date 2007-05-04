<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for operation level class generation. This XSLT will generate wrapper classes and 
	 fault classes for all operations of a given service.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates select="cixs-service/cixs-operation"/></xsl:template>

<xsl:template match="cixs-operation">

	<!-- Define variables used by all templates -->
	<xsl:variable name="operation-class-name">
		<xsl:value-of select="concat(upper-case(substring(operation-name,1,1)),substring(operation-name,2))"/>
	</xsl:variable>
	<xsl:variable name="request-wrapper-type">
		<xsl:choose>
			<xsl:when test="string-length(request-wrapper-type) > 0"><xsl:value-of select="request-wrapper-type"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>Request</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="response-wrapper-type">
		<xsl:choose>
			<xsl:when test="string-length(response-wrapper-type) > 0"><xsl:value-of select="response-wrapper-type"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>Response</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="operation-namespace">
		<xsl:choose>
			<xsl:when test="string-length(operation-targetnamespace) > 0"><xsl:value-of select="operation-targetnamespace"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="../service-targetnamespace"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="operation-package">
		<xsl:choose>
			<xsl:when test="string-length(operation-package) > 0"><xsl:value-of select="operation-package"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="../service-endpoint-package"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="target-dir">
		<xsl:value-of select="translate($operation-package,'.','/')"/>
	</xsl:variable>
	<xsl:variable name="fault-type">
		<xsl:choose>
			<xsl:when test="string-length(fault-type) > 0"><xsl:value-of select="fault-type"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>Fault</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="fault-info-type">
		<xsl:choose>
			<xsl:when test="string-length(fault-info-type) > 0"><xsl:value-of select="fault-info-type"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>FaultInfo</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	
	<xsl:call-template name="generate-input-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
		<xsl:with-param name="operation-class-name"><xsl:value-of select="$operation-class-name"/></xsl:with-param>
		<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		<xsl:with-param name="request-wrapper-type"><xsl:value-of select="$request-wrapper-type"/></xsl:with-param>
		<xsl:with-param name="input-jaxb-type"><xsl:value-of select="input/@jaxb-type"/></xsl:with-param>
		<xsl:with-param name="input-jaxb-package"><xsl:value-of select="input/@jaxb-package"/></xsl:with-param>
	</xsl:call-template>
	
	<xsl:call-template name="generate-output-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
		<xsl:with-param name="operation-class-name"><xsl:value-of select="$operation-class-name"/></xsl:with-param>
		<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		<xsl:with-param name="response-wrapper-type"><xsl:value-of select="$response-wrapper-type"/></xsl:with-param>
		<xsl:with-param name="output-jaxb-type"><xsl:value-of select="output/@jaxb-type"/></xsl:with-param>
		<xsl:with-param name="output-jaxb-package"><xsl:value-of select="output/@jaxb-package"/></xsl:with-param>
	</xsl:call-template>

	<xsl:call-template name="generate-webfault-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
		<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		<xsl:with-param name="fault-type"><xsl:value-of select="$fault-type"/></xsl:with-param>
		<xsl:with-param name="fault-info-type"><xsl:value-of select="$fault-info-type"/></xsl:with-param>
	</xsl:call-template>
	
	<xsl:call-template name="generate-faultinfo-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
		<xsl:with-param name="fault-info-type"><xsl:value-of select="$fault-info-type"/></xsl:with-param>
	</xsl:call-template>
	
</xsl:template>

<!-- ===============================================================================================
	 Generate the input wrapper class
 -->
<xsl:template name="generate-input-wrapper">
	<xsl:param name="target-dir"></xsl:param>
	<xsl:param name="operation-package"></xsl:param>
	<xsl:param name="operation-class-name"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
	<xsl:param name="request-wrapper-type"></xsl:param>
	<xsl:param name="input-jaxb-type"></xsl:param>
	<xsl:param name="input-jaxb-package"></xsl:param>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$request-wrapper-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-wrapper-header">
			<xsl:with-param name="property-name">Request</xsl:with-param>
			<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
			<xsl:with-param name="jaxb-package"><xsl:value-of select="$input-jaxb-package"/></xsl:with-param>
			<xsl:with-param name="jaxb-type"><xsl:value-of select="$input-jaxb-type"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-wrapper-class">
			<xsl:with-param name="wrapper-type"><xsl:value-of select="$request-wrapper-type"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
			<xsl:with-param name="jaxb-type"><xsl:value-of select="$input-jaxb-type"/></xsl:with-param>
			<xsl:with-param name="field-name">request</xsl:with-param>
			<xsl:with-param name="property-name">Request</xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
	 Generate the output wrapper class
 -->
<xsl:template name="generate-output-wrapper">
	<xsl:param name="target-dir"></xsl:param>
	<xsl:param name="operation-package"></xsl:param>
	<xsl:param name="operation-class-name"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
	<xsl:param name="response-wrapper-type"></xsl:param>
	<xsl:param name="output-jaxb-type"></xsl:param>
	<xsl:param name="output-jaxb-package"></xsl:param>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$response-wrapper-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-wrapper-header">
			<xsl:with-param name="property-name">Response</xsl:with-param>
			<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
			<xsl:with-param name="jaxb-package"><xsl:value-of select="$output-jaxb-package"/></xsl:with-param>
			<xsl:with-param name="jaxb-type"><xsl:value-of select="$output-jaxb-type"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-wrapper-class">
			<xsl:with-param name="wrapper-type"><xsl:value-of select="$response-wrapper-type"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
			<xsl:with-param name="jaxb-type"><xsl:value-of select="$output-jaxb-type"/></xsl:with-param>
			<xsl:with-param name="field-name">response</xsl:with-param>
			<xsl:with-param name="property-name">Response</xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
	 Generate the web fault class
 -->
<xsl:template name="generate-webfault-wrapper">
	<xsl:param name="target-dir"></xsl:param>
	<xsl:param name="operation-package"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
	<xsl:param name="fault-type"></xsl:param>
	<xsl:param name="fault-info-type"></xsl:param>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$fault-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-webfault-header">
			<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-webfault-class">
			<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
			<xsl:with-param name="fault-type"><xsl:value-of select="$fault-type"/></xsl:with-param>
			<xsl:with-param name="fault-info-type"><xsl:value-of select="$fault-info-type"/></xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
	 Generate the fault info class
 -->
<xsl:template name="generate-faultinfo-wrapper">
	<xsl:param name="target-dir"></xsl:param>
	<xsl:param name="operation-package"></xsl:param>
	<xsl:param name="fault-info-type"></xsl:param>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$fault-info-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-faultinfo-header">
			<xsl:with-param name="operation-package"><xsl:value-of select="$operation-package"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-faultinfo-class">
			<xsl:with-param name="fault-info-type"><xsl:value-of select="$fault-info-type"/></xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
	 Generate the package and import code for input or output wrapper
 -->
<xsl:template name="generate-wrapper-header">
<xsl:param name="property-name"/>
<xsl:param name="operation-package"/>
<xsl:param name="jaxb-package"/>
<xsl:param name="jaxb-type"/>
package <xsl:value-of select="$operation-package"/>;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
<xsl:if test="string-length($jaxb-package) > 0">
import <xsl:value-of select="$jaxb-package"/>.<xsl:value-of select="$jaxb-type"/>;</xsl:if>

/**
 * <xsl:value-of select="$property-name"/> wrapper element.
 * 
 * This class was generated by CIXS generator.
 * <xsl:value-of  select="current-dateTime()"/>
 */
</xsl:template>

<!-- ===============================================================================================
	 Generate the code of the wrapper java class
 -->
<xsl:template name="generate-wrapper-class">
<xsl:param name="wrapper-type"/>
<xsl:param name="operation-namespace"/>
<xsl:param name="jaxb-type"/>
<xsl:param name="field-name"/>
<xsl:param name="property-name"/>
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "<xsl:value-of select="$wrapper-type"/>",
         namespace = "<xsl:value-of select="$operation-namespace"/>",
         propOrder = {
    "<xsl:value-of select="$field-name"/>" 
})
public class <xsl:value-of select="$wrapper-type"/> {
	
    /** Inner JAXB-bound object. */
    @XmlElement(name = "<xsl:value-of select="$property-name"/>",
                namespace = "<xsl:value-of select="$operation-namespace"/>",
                required = true)
    private <xsl:value-of select="$jaxb-type"/><xsl:text> </xsl:text><xsl:value-of select="$field-name"/>;

	/**
	 * Get the inner JAXB-bound object.
	 * 
	 * @return JAXB-bound object
	 */
	public final <xsl:value-of select="$jaxb-type"/> get<xsl:value-of select="$property-name"/>() {
		return <xsl:value-of select="$field-name"/>;
	}

	/**
	 * Set the inner JAXB-bound object.
	 * 
	 * @param value JAXB-bound object
	 */
	public final void set<xsl:value-of select="$property-name"/>(
	    final <xsl:value-of select="$jaxb-type"/><xsl:text> value</xsl:text>) {
		this.<xsl:value-of select="$field-name"/> = value;
	}
}
</xsl:template>

<!-- ===============================================================================================
	 Generate the package and import code for the webfault class
 -->
<xsl:template name="generate-webfault-header">
<xsl:param name="operation-package"/>
package <xsl:value-of select="$operation-package"/>;
import javax.xml.ws.WebFault;

/**
 * Fault element used as return message when an exception occurs.
 * 
 * This class was generated by CIXS generator.
 * <xsl:value-of  select="current-dateTime()"/>
 */
</xsl:template>

<!-- ===============================================================================================
	 Generate the code of the java exception that wraps the fault
 -->
<xsl:template name="generate-webfault-class">
<xsl:param name="operation-package"/>
<xsl:param name="operation-namespace"/>
<xsl:param name="fault-type"/>
<xsl:param name="fault-info-type"/>
@WebFault(name = "<xsl:value-of select="$fault-info-type"/>",
          targetNamespace = "<xsl:value-of select="$operation-namespace"/>")
public class <xsl:value-of select="$fault-type"/>
    extends Exception {

    /** Default serialVersionUID.  */
	private static final long serialVersionUID = 1L;
	
    /** Java type that goes as soapenv:Fault detail element. */
    private <xsl:value-of select="$fault-info-type"/> faultInfo;

    /**
     * Constructor for Web Fault.
     * @param fault error details
     * @param message error summary
     */
    public <xsl:value-of select="$fault-type"/>(
    	final String message,
    	final <xsl:value-of select="$fault-info-type"/> fault) {
    	
        super(message);
        this.faultInfo = fault;
    }

    /**
     * Constructor for Web Fault with cause.
     * @param fault error details
     * @param message error summary
     * @param cause the cause
     */
    public <xsl:value-of select="$fault-type"/>(
        final String message,
        final <xsl:value-of select="$fault-info-type"/> fault,
        final Throwable cause) {
        
        super(message, cause);
        this.faultInfo = fault;
    }

    /**
     * 
     * @return
     *     returns fault bean: <xsl:value-of select="$operation-package"/>.<xsl:value-of select="$fault-info-type"/>
     */
    public final <xsl:value-of select="$fault-info-type"/> getFaultInfo() {
        return faultInfo;
    }

}
</xsl:template>

<!-- ===============================================================================================
	 Generate the package and import code for the faultInfo class
 -->
<xsl:template name="generate-faultinfo-header">
<xsl:param name="operation-package"/>
package <xsl:value-of select="$operation-package"/>;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

/**
 * Describes an error situation.
 * 
 * This class was generated by CIXS generator.
 * <xsl:value-of  select="current-dateTime()"/>
 */
</xsl:template>

<!-- ===============================================================================================
	 Generate the code of the java faultinfo class
 -->
<xsl:template name="generate-faultinfo-class">
<xsl:param name="fault-info-type"/>
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "<xsl:value-of select="$fault-info-type"/>", propOrder = {
    "detail",
    "message"
})
public class <xsl:value-of select="$fault-info-type"/> {

    /** Detailed fault description. */
    private String detail;

    /** Summary fault description. */
    private String message;

    /**
     * Gets the value of the detail property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public final String getDetail() {
        return detail;
    }

    /**
     * Sets the value of the detail property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public final void setDetail(final String value) {
        this.detail = value;
    }

    /**
     * Gets the value of the message property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public final String getMessage() {
        return message;
    }

    /**
     * Sets the value of the message property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public final void setMessage(final String value) {
        this.message = value;
    }

}
</xsl:template>

</xsl:stylesheet>
