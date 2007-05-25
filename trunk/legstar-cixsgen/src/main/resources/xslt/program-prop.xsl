<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for a program.properties Generation. The resulting properties file has parameters to
	 describe the host target program. This stylesheet uses SAXON extensibility for java calls.
	 The calculation of the host byte size is delegated to a java static method.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:coxb="java:com.legstar.util.JaxbUtil" exclude-result-prefixes="coxb">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates select="//cixsOperation" /></xsl:template>

<xsl:template match="cixsOperation">
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{@name}.properties" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-content"/>
	</xsl:result-document>
</xsl:template>

<!-- ===============================================================================================
	 Generate the content of the program.properties file
 -->
<xsl:template name="generate-content">
# Host Program parameters
# -----------------------
CICSProgramName=<xsl:value-of select="upper-case(@cicsProgramName)"/>
	<xsl:choose>
		<xsl:when test="string-length(@cicsChannel) > 0">
			<xsl:call-template name="generate-container-content"/>
		</xsl:when>
		<xsl:otherwise>
			<xsl:call-template name="generate-commarea-content"/>
		</xsl:otherwise>
	</xsl:choose>
#CICSSysID
#CICSSyncOnReturn
#CICSTransID
</xsl:template>
<!-- ===============================================================================================
	 Generate the content for a commarea-driven program
 -->
<xsl:template name="generate-commarea-content">
<xsl:variable name="qual-input-type"><xsl:value-of select="concat(input/@jaxbPackageName,'.',input/@jaxbType)"/></xsl:variable>
<xsl:variable name="qual-output-type"><xsl:value-of select="concat(output/@jaxbPackageName,'.',output/@jaxbType)"/></xsl:variable>

<!-- Commarea length is the max between input and output host buffer lengths.  -->
<xsl:variable name="input-commarea-len"><xsl:value-of select="coxb:byteLength(input/@jaxbPackageName,input/@jaxbType)"/></xsl:variable>
<xsl:variable name="output-commarea-len">
	<xsl:choose>
		<xsl:when test="string-length($qual-output-type) = 0 or ($qual-input-type = $qual-output-type)"><xsl:value-of select="$input-commarea-len"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="coxb:byteLength(output/@jaxbPackageName,output/@jaxbType)"/></xsl:otherwise>
	</xsl:choose>
</xsl:variable>
<xsl:variable name="commarea-len">
	<xsl:choose>
		<xsl:when test="number($output-commarea-len) > number($input-commarea-len)"><xsl:value-of select="$output-commarea-len"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="$input-commarea-len"/></xsl:otherwise>
	</xsl:choose>
</xsl:variable>
CICSLength=<xsl:value-of select="$commarea-len"/>
CICSDataLength=<xsl:value-of select="$input-commarea-len"/>
</xsl:template>
<!-- ===============================================================================================
	 Generate the content for a container-driven program
 -->
<xsl:template name="generate-container-content">
CICSChannel=<xsl:value-of select="@cicsChannel"/>
	<xsl:for-each select="input[@cicsContainer]">
CICSInContainers_<xsl:value-of select="position()"/>=<xsl:value-of select="@cicsContainer"/>
CICSInContainersLength_<xsl:value-of select="position()"/>=<xsl:value-of select="coxb:byteLength(@jaxbPackageName,@jaxbType)"/>
	</xsl:for-each>
	<xsl:for-each select="output[@cicsContainer]">
CICSOutContainers_<xsl:value-of select="position()"/>=<xsl:value-of select="@cicsContainer"/>
CICSOutContainersLength_<xsl:value-of select="position()"/>=<xsl:value-of select="coxb:byteLength(@jaxbPackageName,@jaxbType)"/>
	</xsl:for-each>
</xsl:template>

</xsl:stylesheet>
