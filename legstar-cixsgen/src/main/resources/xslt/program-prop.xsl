<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
	 XSLT for a program.properties Generation. The resulting properties file has parameters to
	 describe the host target program. This stylesheet uses SAXON extensibility for java calls.
	 The calculation of the commarea size is delegated to a java static method.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:coxb="java:com.legstar.util.JaxbUtil" exclude-result-prefixes="coxb">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>
<xsl:template match="/"><xsl:apply-templates select="//cixs-operation" /></xsl:template>

<xsl:template match="cixs-operation">
	<!-- Generate the dynamically built java source file -->
	<xsl:result-document href="{operation-name}.properties" method="text" omit-xml-declaration="yes" indent="yes">
		<xsl:call-template name="generate-content"/>
	</xsl:result-document>
</xsl:template>

<!-- ===============================================================================================
	 Generate the content of the program.properties file
 -->
<xsl:template name="generate-content">
<xsl:variable name="qual-input-type"><xsl:value-of select="concat(input/@jaxb-package,'.',input/@jaxb-type)"/></xsl:variable>
<xsl:variable name="qual-output-type"><xsl:value-of select="concat(output/@jaxb-package,'.',output/@jaxb-type)"/></xsl:variable>

<!-- Commarea length is the max between input and output host buffer lengths.  -->
<xsl:variable name="input-commarea-len"><xsl:value-of select="coxb:byteLength(input/@jaxb-package,input/@jaxb-type)"/></xsl:variable>
<xsl:variable name="output-commarea-len">
	<xsl:choose>
		<xsl:when test="string-length($qual-output-type) = 0 or ($qual-input-type = $qual-output-type)"><xsl:value-of select="$input-commarea-len"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="coxb:byteLength(output/@jaxb-package,output/@jaxb-type)"/></xsl:otherwise>
	</xsl:choose>
</xsl:variable>
<xsl:variable name="commarea-len">
	<xsl:choose>
		<xsl:when test="number($output-commarea-len) > number($input-commarea-len)"><xsl:value-of select="$output-commarea-len"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="$input-commarea-len"/></xsl:otherwise>
	</xsl:choose>
</xsl:variable>
# Host Program parameters
# -----------------------
ProgramName=<xsl:value-of select="upper-case(program-name)"/>
ExecuteTimeout=5000
CommareaLength=<xsl:value-of select="$commarea-len"/>
#DataLength
#SysID
#SyncOnReturn
#TransID
</xsl:template>

</xsl:stylesheet>
