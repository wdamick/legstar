<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for a sun-jaxws.xml Generation. This configuration file is used by JAXWS to
	 locate the Web Service Endpoint implementation.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" omit-xml-declaration="no" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates  select="cixsService"/></xsl:template>
<xsl:param name="xslt.param"/>

<xsl:template match="cixsService">
	<xsl:call-template name="generate-content"/>
</xsl:template>

<!-- ===============================================================================================
	 Generate the content of the sun-jaxws.xml file
 -->
<xsl:template name="generate-content">

	<!--  Provide default values for missing parameters -->
	<xsl:variable name="servlet-name"><xsl:value-of select="@name"/>WebService</xsl:variable>
	<xsl:variable name="servlet-url-pattern">/<xsl:value-of select="@name"/></xsl:variable>
	<xsl:variable name="implementation-class-name">
		<xsl:choose>
			<xsl:when test="string-length(@endpointImplementationClassName) > 0"><xsl:value-of select="@endpointImplementationClassName"/></xsl:when>
			<xsl:otherwise><xsl:value-of select="concat(upper-case(substring(@name,1,1)),substring(@name,2))"/>Impl</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	
	<!--  Create content -->
	<xsl:element  name = "endpoints" namespace="http://java.sun.com/xml/ns/jax-ws/ri/runtime">
    	<xsl:attribute  name = "version" >2.0</xsl:attribute>
		<xsl:element  name = "endpoint" namespace="http://java.sun.com/xml/ns/jax-ws/ri/runtime">
	    	<xsl:attribute  name = "name" ><xsl:value-of select="$servlet-name"/></xsl:attribute>
	    	<xsl:attribute  name = "implementation" ><xsl:value-of select="@endpointPackageName"/>.<xsl:value-of select="$implementation-class-name"/></xsl:attribute>
	    	<xsl:attribute  name = "url-pattern" ><xsl:value-of select="$servlet-url-pattern"/></xsl:attribute>
		</xsl:element>
   </xsl:element>
</xsl:template>

</xsl:stylesheet>
