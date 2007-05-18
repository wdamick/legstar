<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for Service Endpoint Generation. Endpoint is compatible with JAXWS.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates select="cixs-service"/></xsl:template>

<!-- Generate the service endpoint interface class -->
<xsl:template match="cixs-service">
	<!-- Determine the endpoint interface java source file name -->
	<xsl:variable name="sei-class-name">
		<xsl:choose>
			<xsl:when test="string-length(endpoint-interface) > 0"><xsl:value-of select="endpoint-interface"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="concat(upper-case(substring(service-name,1,1)),substring(service-name,2))"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="target-dir">
		<xsl:value-of select="translate(service-endpoint-package,'.','/')"/>
	</xsl:variable>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$sei-class-name}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-header"/>
		<xsl:call-template name="generate-interface">
			<xsl:with-param name="sei-class-name"><xsl:value-of select="$sei-class-name"/></xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
	 Generate the package and import code
 -->
<xsl:template name="generate-header">
package <xsl:value-of select="service-endpoint-package"/>;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

<xsl:for-each select="cixs-operation">

<!-- Import the operation wrapper objects packages -->
<xsl:if test="string-length(operation-package) > 0 and (operation-package != ../service-endpoint-package)">
import <xsl:value-of select="operation-package"/>.*;</xsl:if>

</xsl:for-each>
/**
 * Web service endpoint interface.
 * 
 * This class was generated by CIXS generator.
 * <xsl:value-of  select="current-dateTime()"/>
 */
</xsl:template>

<!-- ===============================================================================================
	 Generate the code of the java interface
 -->
<xsl:template name="generate-interface">
<xsl:param name="sei-class-name"/>
<xsl:variable name="wsdl-port-type">
	<xsl:choose>
		<xsl:when test="string-length(wsdl-port-type) > 0"><xsl:value-of select="wsdl-port-type"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="service-name"/>Port</xsl:otherwise>
	</xsl:choose>
</xsl:variable>
@WebService(name = "<xsl:value-of select="$wsdl-port-type"/>",
            targetNamespace = "<xsl:value-of select="service-targetnamespace"/>")
public interface <xsl:value-of select="$sei-class-name"/> {

<xsl:for-each select="cixs-operation">

<xsl:variable name="operation-class-name">
	<xsl:value-of select="concat(upper-case(substring(operation-name,1,1)),substring(operation-name,2))"/>
</xsl:variable>
<xsl:variable name="operation-package">
	<xsl:choose>
		<xsl:when test="string-length(operation-package) > 0"><xsl:value-of select="operation-package"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="../service-endpoint-package"/></xsl:otherwise>
	</xsl:choose>
</xsl:variable>
<xsl:variable name="operation-namespace">
	<xsl:choose>
		<xsl:when test="string-length(operation-targetnamespace) > 0"><xsl:value-of select="operation-targetnamespace"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="../service-targetnamespace"/></xsl:otherwise>
	</xsl:choose>
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
<xsl:variable name="request-holder-type">
	<xsl:choose>
		<xsl:when test="string-length(request-holder-type) > 0"><xsl:value-of select="request-holder-type"/></xsl:when>
		<xsl:otherwise>
			<xsl:choose>
				<xsl:when test="count(input) > 1"><xsl:value-of select="$operation-class-name"/>RequestHolder</xsl:when>
				<xsl:otherwise><xsl:value-of select="input/@jaxb-package"/>.<xsl:value-of select="input/@jaxb-type"/></xsl:otherwise>
			</xsl:choose>
		</xsl:otherwise>
	</xsl:choose>
</xsl:variable>
<xsl:variable name="response-holder-type">
	<xsl:choose>
		<xsl:when test="string-length(response-holder-type) > 0"><xsl:value-of select="response-holder-type"/></xsl:when>
		<xsl:otherwise>
			<xsl:choose>
				<xsl:when test="count(output) > 1"><xsl:value-of select="$operation-class-name"/>ResponseHolder</xsl:when>
				<xsl:otherwise><xsl:value-of select="output/@jaxb-package"/>.<xsl:value-of select="output/@jaxb-type"/></xsl:otherwise>
			</xsl:choose>
		</xsl:otherwise>
	</xsl:choose>
</xsl:variable>
<xsl:variable name="fault-type">
	<xsl:choose>
		<xsl:when test="string-length(fault-type) > 0"><xsl:value-of select="fault-type"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>Fault</xsl:otherwise>
	</xsl:choose>
</xsl:variable>
    /**
     * Service operation <xsl:value-of select="operation-name"/>.
     * 
     * @param request a JAXB object mapping the request
     * @param hostHeader a JAXB object mapping header parameters
     * @return a JAXB object mapping the reply
     * @throws <xsl:value-of select="$fault-type"/> if method fails
     */
    @WebMethod
    @WebResult(name = "Response",
        targetNamespace = "<xsl:value-of select="$operation-namespace"/>")
    @RequestWrapper(localName = "<xsl:value-of select="$request-wrapper-type"/>",
        targetNamespace = "<xsl:value-of select="$operation-namespace"/>",
        className = "<xsl:value-of select="$operation-package"/>.<xsl:value-of select="$request-wrapper-type"/>")
    @ResponseWrapper(localName = "<xsl:value-of select="$response-wrapper-type"/>",
        targetNamespace = "<xsl:value-of select="$operation-namespace"/>",
        className = "<xsl:value-of select="$operation-package"/>.<xsl:value-of select="$response-wrapper-type"/>")
    <xsl:value-of select="$response-holder-type"/><xsl:text> </xsl:text><xsl:value-of select="operation-name"/>(
        @WebParam(name = "Request",
               targetNamespace = "<xsl:value-of select="$operation-namespace"/>")
        <xsl:value-of select="$request-holder-type"/> request,
        @WebParam(name = "HostHeader", header = true, partName = "HostHeader",
                targetNamespace = "<xsl:value-of select="$operation-namespace"/>")
        <xsl:value-of select="$sei-class-name"/>HostHeader hostHeader)
        throws <xsl:value-of select="$fault-type"/>;
</xsl:for-each>
}

</xsl:template>

</xsl:stylesheet>
