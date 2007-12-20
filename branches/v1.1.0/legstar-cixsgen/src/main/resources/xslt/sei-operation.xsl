<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for operation level class generation. This XSLT will generate wrapper classes and 
	 fault classes for all operations of a given service.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates select="cixsService/cixsOperation"/></xsl:template>

<xsl:template match="cixsOperation">

	<!-- Define variables used by all templates -->
	<xsl:variable name="operation-class-name">
		<xsl:value-of select="concat(upper-case(substring(@name,1,1)),substring(@name,2))"/>
	</xsl:variable>
	<xsl:variable name="request-wrapper-type">
		<xsl:choose>
			<xsl:when test="string-length(@requestWrapperType) > 0"><xsl:value-of select="@requestWrapperType"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>Request</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="response-wrapper-type">
		<xsl:choose>
			<xsl:when test="string-length(@responseWrapperType) > 0"><xsl:value-of select="@responseWrapperType"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>Response</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="operation-namespace">
		<xsl:choose>
			<xsl:when test="string-length(@namespace) > 0"><xsl:value-of select="@namespace"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="../@targetNamespace"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="operation-package-name">
		<xsl:choose>
			<xsl:when test="string-length(@packageName) > 0"><xsl:value-of select="@packageName"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="../@endpointPackageName"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="target-dir">
		<xsl:value-of select="translate($operation-package-name,'.','/')"/>
	</xsl:variable>
	<xsl:variable name="fault-type">
		<xsl:choose>
			<xsl:when test="string-length(@faultType) > 0"><xsl:value-of select="@faultType"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>Fault</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="fault-info-type">
		<xsl:choose>
			<xsl:when test="string-length(@faultInfoType) > 0"><xsl:value-of select="@faultInfoType"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="$operation-class-name"/>FaultInfo</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
    <xsl:variable name="multiple-input">
        <xsl:choose>
            <xsl:when test="count(input) > 1">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="multiple-output">
        <xsl:choose>
            <xsl:when test="count(output) > 1">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
	
	<xsl:if test="$multiple-input = 'true'">
		<xsl:call-template name="generate-input-holder">
			<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
			<xsl:with-param name="operation-class-name"><xsl:value-of select="$operation-class-name"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		</xsl:call-template>
	</xsl:if>
	
	<xsl:if test="$multiple-output = 'true'">
		<xsl:call-template name="generate-output-holder">
			<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
			<xsl:with-param name="operation-class-name"><xsl:value-of select="$operation-class-name"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		</xsl:call-template>
	</xsl:if>
	
	<xsl:call-template name="generate-input-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		<xsl:with-param name="operation-class-name"><xsl:value-of select="$operation-class-name"/></xsl:with-param>
		<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		<xsl:with-param name="request-wrapper-type"><xsl:value-of select="$request-wrapper-type"/></xsl:with-param>
		<xsl:with-param name="multiple-input"><xsl:value-of select="$multiple-input"/></xsl:with-param>
	</xsl:call-template>
	
	<xsl:call-template name="generate-output-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		<xsl:with-param name="operation-class-name"><xsl:value-of select="$operation-class-name"/></xsl:with-param>
		<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		<xsl:with-param name="response-wrapper-type"><xsl:value-of select="$response-wrapper-type"/></xsl:with-param>
		<xsl:with-param name="multiple-output"><xsl:value-of select="$multiple-output"/></xsl:with-param>
	</xsl:call-template>

	<xsl:call-template name="generate-webfault-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
		<xsl:with-param name="fault-type"><xsl:value-of select="$fault-type"/></xsl:with-param>
		<xsl:with-param name="fault-info-type"><xsl:value-of select="$fault-info-type"/></xsl:with-param>
	</xsl:call-template>
	
	<xsl:call-template name="generate-faultinfo-wrapper">
		<xsl:with-param name="target-dir"><xsl:value-of select="$target-dir"/></xsl:with-param>
		<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		<xsl:with-param name="fault-info-type"><xsl:value-of select="$fault-info-type"/></xsl:with-param>
	</xsl:call-template>
	
</xsl:template>

<!-- ===============================================================================================
	 Generate the input holder class
 -->
<xsl:template name="generate-input-holder">
	<xsl:param name="target-dir"></xsl:param>
	<xsl:param name="operation-package-name"></xsl:param>
	<xsl:param name="operation-class-name"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
 
    <xsl:variable name="request-holder-type"><xsl:value-of select="$operation-class-name"/>RequestHolder</xsl:variable>

	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$request-holder-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-holder-header">
			<xsl:with-param name="property-name">Request</xsl:with-param>
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-holder-class">
			<xsl:with-param name="holder-type"><xsl:value-of select="$request-holder-type"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
			<xsl:with-param name="property-name">Request</xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
	 Generate the output holder class
 -->
<xsl:template name="generate-output-holder">
	<xsl:param name="target-dir"></xsl:param>
	<xsl:param name="operation-package-name"></xsl:param>
	<xsl:param name="operation-class-name"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
 
    <xsl:variable name="response-holder-type"><xsl:value-of select="$operation-class-name"/>ResponseHolder</xsl:variable>

	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$response-holder-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-holder-header">
			<xsl:with-param name="property-name">Response</xsl:with-param>
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-holder-class">
			<xsl:with-param name="holder-type"><xsl:value-of select="$response-holder-type"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
			<xsl:with-param name="property-name">Response</xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>


<!-- ===============================================================================================
	 Generate the input wrapper class
 -->
<xsl:template name="generate-input-wrapper">
	<xsl:param name="target-dir"></xsl:param>
	<xsl:param name="operation-package-name"></xsl:param>
	<xsl:param name="operation-class-name"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
	<xsl:param name="request-wrapper-type"></xsl:param>
	<xsl:param name="multiple-input"></xsl:param>

    <xsl:variable name="import">
        <xsl:choose>
            <xsl:when test="$multiple-input != 'true' and string-length(input/@jaxbPackageName) > 0"><xsl:value-of select="input/@jaxbPackageName"/>.<xsl:value-of select="input/@jaxbType"/></xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="field-type">
        <xsl:choose>
            <xsl:when test="$multiple-input = 'true'"><xsl:value-of select="$operation-class-name"/>RequestHolder</xsl:when>
            <xsl:otherwise><xsl:value-of select="input/@jaxbType"/></xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$request-wrapper-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-wrapper-header">
			<xsl:with-param name="property-name">Request</xsl:with-param>
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
			<xsl:with-param name="import"><xsl:value-of select="$import"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-wrapper-class">
			<xsl:with-param name="wrapper-type"><xsl:value-of select="$request-wrapper-type"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
			<xsl:with-param name="field-type"><xsl:value-of select="$field-type"/></xsl:with-param>
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
	<xsl:param name="operation-package-name"></xsl:param>
	<xsl:param name="operation-class-name"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
	<xsl:param name="response-wrapper-type"></xsl:param>
	<xsl:param name="multiple-output"></xsl:param>
	
    <xsl:variable name="import">
        <xsl:choose>
            <xsl:when test="$multiple-output != 'true' and string-length(output/@jaxbPackageName) > 0"><xsl:value-of select="output/@jaxbPackageName"/>.<xsl:value-of select="output/@jaxbType"/></xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
    <xsl:variable name="field-type">
        <xsl:choose>
            <xsl:when test="$multiple-output = 'true'"><xsl:value-of select="$operation-class-name"/>ResponseHolder</xsl:when>
            <xsl:otherwise><xsl:value-of select="output/@jaxbType"/></xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$response-wrapper-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-wrapper-header">
			<xsl:with-param name="property-name">Response</xsl:with-param>
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
			<xsl:with-param name="import"><xsl:value-of select="$import"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-wrapper-class">
			<xsl:with-param name="wrapper-type"><xsl:value-of select="$response-wrapper-type"/></xsl:with-param>
			<xsl:with-param name="operation-namespace"><xsl:value-of select="$operation-namespace"/></xsl:with-param>
			<xsl:with-param name="field-type"><xsl:value-of select="$field-type"/></xsl:with-param>
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
	<xsl:param name="operation-package-name"></xsl:param>
	<xsl:param name="operation-namespace"></xsl:param>
	<xsl:param name="fault-type"></xsl:param>
	<xsl:param name="fault-info-type"></xsl:param>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$fault-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-webfault-header">
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-webfault-class">
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
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
	<xsl:param name="operation-package-name"></xsl:param>
	<xsl:param name="fault-info-type"></xsl:param>
	
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{$target-dir}/{$fault-info-type}.java" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-faultinfo-header">
			<xsl:with-param name="operation-package-name"><xsl:value-of select="$operation-package-name"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="generate-faultinfo-class">
			<xsl:with-param name="fault-info-type"><xsl:value-of select="$fault-info-type"/></xsl:with-param>
		</xsl:call-template>
	</xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
	 Generate the package and import code for input or output holder. A holder is composed by
	 multiple jaxb object trees. 
 -->
<xsl:template name="generate-holder-header">
<xsl:param name="operation-package-name"/>
<xsl:param name="property-name"/>
package <xsl:value-of select="$operation-package-name"/>;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
<xsl:choose>
	<xsl:when test="$property-name = 'Request'">
		<xsl:for-each select="input">
import <xsl:value-of select="@jaxbPackageName"/>.<xsl:value-of select="@jaxbType"/>;
		</xsl:for-each>
	</xsl:when>
	<xsl:when test="$property-name = 'Response'">
		<xsl:for-each select="output">
import <xsl:value-of select="@jaxbPackageName"/>.<xsl:value-of select="@jaxbType"/>;
		</xsl:for-each>
	</xsl:when>
</xsl:choose>

/**
 * <xsl:value-of select="$property-name"/> holder element.
 * 
 * This class was generated by CIXS generator.
 * <xsl:value-of  select="current-dateTime()"/>
 */
</xsl:template>

<!-- ===============================================================================================
	 Generate the code of the holder java class
 -->
<xsl:template name="generate-holder-class">
<xsl:param name="holder-type"/>
<xsl:param name="operation-namespace"/>
<xsl:param name="property-name"/>
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "<xsl:value-of select="$holder-type"/>",
         namespace = "<xsl:value-of select="$operation-namespace"/>",
         propOrder = {
<xsl:choose>
	<xsl:when test="$property-name = 'Request'">
		<xsl:for-each select="input">
    "<xsl:value-of select="@jaxbFieldName"/>"<xsl:if test="position() &lt; last()">,</xsl:if> 
		</xsl:for-each>
	</xsl:when>
	<xsl:when test="$property-name = 'Response'">
		<xsl:for-each select="output">
    "<xsl:value-of select="@jaxbFieldName"/>"<xsl:if test="position() &lt; last()">,</xsl:if> 
		</xsl:for-each>
	</xsl:when>
</xsl:choose>
})
public class <xsl:value-of select="$holder-type"/> {
	
<xsl:choose>
	<xsl:when test="$property-name = 'Request'">
		<xsl:for-each select="input">
    /** Inner <xsl:value-of select="@jaxbType"/> JAXB-bound object. */
    @XmlElement(name = "<xsl:value-of select="@jaxbFieldName"/>",
                namespace = "<xsl:value-of select="$operation-namespace"/>",
                required = true)
    private <xsl:value-of select="@jaxbType"/><xsl:text> </xsl:text><xsl:value-of select="@jaxbFieldName"/>;
		</xsl:for-each>
	</xsl:when>
	<xsl:when test="$property-name = 'Response'">
		<xsl:for-each select="output">
    /** Inner <xsl:value-of select="@jaxbType"/> JAXB-bound object. */
    @XmlElement(name = "<xsl:value-of select="@jaxbFieldName"/>",
                namespace = "<xsl:value-of select="$operation-namespace"/>",
                required = true)
    private <xsl:value-of select="@jaxbType"/><xsl:text> </xsl:text><xsl:value-of select="@jaxbFieldName"/>;
		</xsl:for-each>
	</xsl:when>
</xsl:choose>

<xsl:choose>
	<xsl:when test="$property-name = 'Request'">
		<xsl:for-each select="input">
	/**
	 * Get the inner <xsl:value-of select="@jaxbType"/> JAXB-bound object.
	 * 
	 * @return JAXB-bound object
	 */
	public final <xsl:value-of select="@jaxbType"/> get<xsl:value-of select="@jaxbPropertyName"/>() {
		return <xsl:value-of select="@jaxbFieldName"/>;
	}

	/**
	 * Set the inner <xsl:value-of select="@jaxbType"/> JAXB-bound object.
	 * 
	 * @param value JAXB-bound object
	 */
	public final void set<xsl:value-of select="@jaxbPropertyName"/>(
	    final <xsl:value-of select="@jaxbType"/><xsl:text> value</xsl:text>) {
		this.<xsl:value-of select="@jaxbFieldName"/> = value;
	}
		</xsl:for-each>
	</xsl:when>
	<xsl:when test="$property-name = 'Response'">
		<xsl:for-each select="output">
	/**
	 * Get the inner <xsl:value-of select="@jaxbType"/> JAXB-bound object.
	 * 
	 * @return JAXB-bound object
	 */
	public final <xsl:value-of select="@jaxbType"/> get<xsl:value-of select="@jaxbPropertyName"/>() {
		return <xsl:value-of select="@jaxbFieldName"/>;
	}

	/**
	 * Set the inner <xsl:value-of select="@jaxbType"/> JAXB-bound object.
	 * 
	 * @param value JAXB-bound object
	 */
	public final void set<xsl:value-of select="@jaxbPropertyName"/>(
	    final <xsl:value-of select="@jaxbType"/><xsl:text> value</xsl:text>) {
		this.<xsl:value-of select="@jaxbFieldName"/> = value;
	}
		</xsl:for-each>
	</xsl:when>
</xsl:choose>
}
</xsl:template>

<!-- ===============================================================================================
	 Generate the package and import code for input or output wrapper
 -->
<xsl:template name="generate-wrapper-header">
<xsl:param name="operation-package-name"/>
<xsl:param name="property-name"/>
<xsl:param name="import"/>
package <xsl:value-of select="$operation-package-name"/>;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
<xsl:if test="string-length($import) > 0">
import <xsl:value-of select="$import"/>;
</xsl:if>

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
<xsl:param name="field-type"/>
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
    private <xsl:value-of select="$field-type"/><xsl:text> </xsl:text><xsl:value-of select="$field-name"/>;

	/**
	 * Get the inner JAXB-bound object.
	 * 
	 * @return JAXB-bound object
	 */
	public final <xsl:value-of select="$field-type"/> get<xsl:value-of select="$property-name"/>() {
		return <xsl:value-of select="$field-name"/>;
	}

	/**
	 * Set the inner JAXB-bound object.
	 * 
	 * @param value JAXB-bound object
	 */
	public final void set<xsl:value-of select="$property-name"/>(
	    final <xsl:value-of select="$field-type"/><xsl:text> value</xsl:text>) {
		this.<xsl:value-of select="$field-name"/> = value;
	}
}
</xsl:template>

<!-- ===============================================================================================
	 Generate the package and import code for the webfault class
 -->
<xsl:template name="generate-webfault-header">
<xsl:param name="operation-package-name"/>
package <xsl:value-of select="$operation-package-name"/>;
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
<xsl:param name="operation-package-name"/>
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
     *     returns fault bean: <xsl:value-of select="$operation-package-name"/>.<xsl:value-of select="$fault-info-type"/>
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
<xsl:param name="operation-package-name"/>
package <xsl:value-of select="$operation-package-name"/>;
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
