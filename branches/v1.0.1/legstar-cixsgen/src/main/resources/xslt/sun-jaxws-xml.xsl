<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for a sun-jaxws.xml Generation. This configuration file is used by JAXWS to
	 locate the Web Service Endpoint implementation.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" omit-xml-declaration="no" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates  select="cixs-service"/></xsl:template>
<xsl:param name="xslt.param"/>

<xsl:template match="cixs-service">
	<xsl:call-template name="generate-content"/>
</xsl:template>

<!-- ===============================================================================================
	 Generate the content of the sun-jaxws.xml file
 -->
<xsl:template name="generate-content">

	<!--  Provide default values for missing parameters -->
	<xsl:variable name="servlet-name">
		<xsl:choose>
			<xsl:when test="string-length(servlet-name) > 0"><xsl:value-of select="servlet-name"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="service-name"/>WebService</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="servlet-url-pattern">
		<xsl:choose>
			<xsl:when test="string-length(servlet-url-pattern) > 0"><xsl:value-of select="servlet-url-pattern"/></xsl:when>
			<xsl:otherwise>/<xsl:value-of select="service-name"/></xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="endpoint-implementation">
		<xsl:choose>
			<xsl:when test="string-length(endpoint-implementation) > 0"><xsl:value-of select="endpoint-implementation"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="concat(upper-case(substring(service-name,1,1)),substring(service-name,2))"/>Impl</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	
	<!--  Create content -->
	<xsl:element  name = "endpoints" namespace="http://java.sun.com/xml/ns/jax-ws/ri/runtime">
    	<xsl:attribute  name = "version" >2.0</xsl:attribute>
		<xsl:element  name = "endpoint" namespace="http://java.sun.com/xml/ns/jax-ws/ri/runtime">
	    	<xsl:attribute  name = "name" ><xsl:value-of select="$servlet-name"/></xsl:attribute>
	    	<xsl:attribute  name = "implementation" ><xsl:value-of select="service-endpoint-package"/>.<xsl:value-of select="$endpoint-implementation"/></xsl:attribute>
	    	<xsl:attribute  name = "url-pattern" ><xsl:value-of select="$servlet-url-pattern"/></xsl:attribute>
		</xsl:element>
   </xsl:element>
</xsl:template>

</xsl:stylesheet>
