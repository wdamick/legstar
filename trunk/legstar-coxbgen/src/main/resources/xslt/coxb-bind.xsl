<?xml version="1.0" encoding="UTF-8"?>
<!-- ===============================================================================================
   XSLT for Type binding Generation. For each JAXB complex type with Cobol annotations, this 
   generates runtime binding code which is much faster than reflection and annotation APIs.
 -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="yes"/>

<xsl:param name="marshal-choice-strategy-classname-exists"/>
<xsl:param name="unmarshal-choice-strategy-classname-exists"/>

<xsl:template match="/">
    <xsl:apply-templates select="coxb-type" />
  <xsl:apply-templates select="coxb-type[string-length(@marshalChoiceStrategyClassName) > 0]" mode="choiceStrategy">
     <xsl:with-param name="choice-strategy-class-qual-name" select="coxb-type/@marshalChoiceStrategyClassName"/>
     <xsl:with-param name="choice-strategy-class-exists" select="$marshal-choice-strategy-classname-exists"/>
  </xsl:apply-templates>
  <xsl:apply-templates select="coxb-type[string-length(@unmarshalChoiceStrategyClassName) > 0]" mode="choiceStrategy">
     <xsl:with-param name="choice-strategy-class-qual-name" select="coxb-type/@unmarshalChoiceStrategyClassName"/>
     <xsl:with-param name="choice-strategy-class-exists" select="$unmarshal-choice-strategy-classname-exists"/>
  </xsl:apply-templates>
</xsl:template>

<!-- Generate the JAXB binding class -->
<xsl:template match="coxb-type">

  <!-- Determine the binding java source file name and location -->
  <xsl:variable name="binding-class-name">
    <xsl:value-of select="@name"/>
  </xsl:variable>
  <xsl:variable name="binding-type-package">
    <xsl:value-of select="concat(jaxb-type-package,'.bind')"/>
  </xsl:variable>
  <xsl:variable name="target-dir">
    <xsl:value-of select="translate($binding-type-package,'.','/')"/>
  </xsl:variable>
  
  <!-- Generate the dynamically built java source file -->
  <xsl:result-document href="{$target-dir}/{$binding-class-name}.java" method="text" omit-xml-declaration="yes" indent="yes">
    <xsl:call-template name="generate-header">
      <xsl:with-param name="binding-type-package"><xsl:value-of select="$binding-type-package"/></xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="generate-class">
      <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
    </xsl:call-template>
  </xsl:result-document>

</xsl:template>

<!-- ===============================================================================================
   Generate the package and import code
 -->
<xsl:template name="generate-header">
<xsl:param name="binding-type-package"/>
package <xsl:value-of select="$binding-type-package"/>;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolComplexBinding;
<xsl:if test="@type='complex' or @type='choice'">
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;</xsl:if>
<xsl:if test="@type='complex'">
import com.legstar.coxb.impl.CComplexBinding;</xsl:if>
<xsl:if test="@type='choice'">
import com.legstar.coxb.impl.CChoiceBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CBinaryBinding']) > 0">
import com.legstar.coxb.impl.CBinaryBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CDoubleBinding']) > 0">
import com.legstar.coxb.impl.CDoubleBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CFloatBinding']) > 0">
import com.legstar.coxb.impl.CFloatBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'COctetStreamBinding']) > 0">
import com.legstar.coxb.impl.COctetStreamBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CStringBinding']) > 0">
import com.legstar.coxb.impl.CStringBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CNationalBinding']) > 0">
import com.legstar.coxb.impl.CNationalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CPackedDecimalBinding']) > 0">
import com.legstar.coxb.impl.CPackedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CZonedDecimalBinding']) > 0">
import com.legstar.coxb.impl.CZonedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayBinaryBinding']) > 0">
import com.legstar.coxb.impl.CArrayBinaryBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayDoubleBinding']) > 0">
import com.legstar.coxb.impl.CArrayDoubleBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayFloatBinding']) > 0">
import com.legstar.coxb.impl.CArrayFloatBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayOctetStreamBinding']) > 0">
import com.legstar.coxb.impl.CArrayOctetStreamBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayStringBinding']) > 0">
import com.legstar.coxb.impl.CArrayStringBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayNationalBinding']) > 0">
import com.legstar.coxb.impl.CArrayNationalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayPackedDecimalBinding']) > 0">
import com.legstar.coxb.impl.CArrayPackedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@binding-type = 'CArrayZonedDecimalBinding']) > 0">
import com.legstar.coxb.impl.CArrayZonedDecimalBinding;</xsl:if>
<xsl:if test="count(coxb-property[@jaxb-type = 'BigDecimal']) > 0">
import java.math.BigDecimal;</xsl:if>
<xsl:if test="count(coxb-property[@jaxb-type = 'BigInteger']) > 0">
import java.math.BigInteger;</xsl:if>
<xsl:if test="@type = 'complexArray'">
import com.legstar.coxb.impl.CArrayComplexBinding;</xsl:if>
<xsl:if test="@type = 'complexArray' or count(coxb-property[@type = 'complexArray' or @cobol-maxOccurs > 0]) > 0">
<xsl:value-of select="@jaxb-type"/>
import java.util.List;</xsl:if>
<xsl:if test="@type = 'complexArray'">
import java.util.ArrayList;</xsl:if>
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import <xsl:value-of select="jaxb-type-package"/>.<xsl:value-of select="jaxb-type-name"/>;
import <xsl:value-of select="jaxb-type-package"/>.ObjectFactory;
<xsl:if test="@type='complex' or @type='choice'">
<xsl:for-each select="coxb-property[@type = 'complex' or @type = 'complexArray']">
import <xsl:value-of select="../jaxb-type-package"/>.<xsl:value-of select="@jaxb-type"/>;
</xsl:for-each>
</xsl:if>

/**
 * This class was generated by LegStar coxbgen version 1.1.
 * <xsl:value-of  select="current-dateTime()"/>
</xsl:template>

<!-- ===============================================================================================
   Determine which template applies based on the binding type
 -->
<xsl:template name="generate-class">
<xsl:param name="binding-class-name"/>
    <xsl:choose>
        <xsl:when test="@type = 'choice'">
            <xsl:call-template name="generate-choice-class">
                <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
            </xsl:call-template>
        </xsl:when>
        <xsl:when test="@type = 'complexArray'">
            <xsl:call-template name="generate-complex-array-class">
                <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:call-template name="generate-complex-class">
                <xsl:with-param name="binding-class-name"><xsl:value-of select="$binding-class-name"/></xsl:with-param>
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements a complex binding
 -->
<xsl:template name="generate-complex-class">
<xsl:param name="binding-class-name"/>
 * This class implements a bi-directional binding between a cobol structure and
 * a java object.
 */

public class <xsl:value-of select="$binding-class-name"/> 
             extends CComplexBinding {

    /** Reference to a Jaxb object factory. */
    private ObjectFactory mJaxbObjectFactory;
    
    /** Jaxb object to which this cobol complex element is bound. */
    private <xsl:value-of select="jaxb-type-name"/> mJaxbObject;
  
    /** Indicates that the associated Jaxb object just came from the constructor
     * and doesn't need to be recreated. */
    private boolean mUnusedJaxbObject = false;
    
    /** Children of this complex binding. */
    <xsl:for-each select="coxb-property">
    /** Child bound to jaxb property <xsl:value-of select="@jaxb-name"/>(<xsl:value-of select="@jaxb-type"/>). */
    public <xsl:value-of select="@binding-type"/><xsl:text> </xsl:text><xsl:value-of select="@jaxb-name"/><xsl:if test="@type = 'complexArray'">Wrapper</xsl:if>;</xsl:for-each>
    <xsl:for-each select="coxb-property[@type = 'complexArray']">
    /** Binding item for complex array binding <xsl:value-of select="@binding-type"/>. */
    public <xsl:value-of select="@item-binding-type"/><xsl:text> </xsl:text><xsl:value-of select="@jaxb-name"/>;</xsl:for-each>
            
    /** Logger. */
    private static final Log LOG
        = LogFactory.getLog(<xsl:value-of select="$binding-class-name"/>.class);

    /**
     * Constructor for a root Complex element with a bound JAXB object.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     * @param jaxbObject the concrete JAXB object instance bound to this
     *        complex element
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final ObjectFactory jaxbObjectFactory,
            final <xsl:value-of select="jaxb-type-name"/> jaxbObject) {
        
        this(jaxbObjectFactory, jaxbObject, null, "<xsl:value-of select="jaxb-property-name"/>");
    }

    /**
     * Constructor for a Complex element as a child of another element and
     * an associated JAXB object.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     * @param jaxbObject the concrete JAXB object instance bound to this
     *        complex element
     * @param parentBinding a reference to the parent binding
     * @param jaxbName name of field in parent JAXB object
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final ObjectFactory jaxbObjectFactory,
            final <xsl:value-of select="jaxb-type-name"/> jaxbObject,
            final ICobolComplexBinding parentBinding,
            final String jaxbName) {
        
        super(jaxbName, <xsl:value-of select="jaxb-type-name"/>.class, parentBinding);
        mJaxbObject = jaxbObject;
        mUnusedJaxbObject = true;
        mJaxbObjectFactory = jaxbObjectFactory;
        initChildren();
    }

    /**
     * Constructor for a root Complex element without a bound JAXB object.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final ObjectFactory jaxbObjectFactory) {
        
        this(jaxbObjectFactory, null, "<xsl:value-of select="jaxb-property-name"/>");
    }

    /**
     * Constructor for a child Complex element without a bound JAXB object.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     * @param parentBinding a reference to the parent binding
     * @param jaxbName name of field in parent JAXB object
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final ObjectFactory jaxbObjectFactory,
            final ICobolComplexBinding parentBinding,
            final String jaxbName) {
        
        super(jaxbName, <xsl:value-of select="jaxb-type-name"/>.class, parentBinding);
        mJaxbObjectFactory = jaxbObjectFactory;
        initChildren();
    }

    /** Creates a binding property for each child. */
    public final void initChildren() {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing started");
        }
        /* Create binding children instances */
        <xsl:apply-templates select="coxb-property" mode="generate-property-init"/>
        /* Add children to children list */<xsl:for-each select="coxb-property">
        getChildrenList().add(<xsl:value-of select="@jaxb-name"/><xsl:if test="@type = 'complexArray'">Wrapper</xsl:if>);</xsl:for-each>

        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing successful");
        }
    }

    /** {@inheritDoc} */
    public final void createJaxbObject() throws HostException {
        /* Since this complex binding has a constructor that takes a
         * JAXB object, we might already have a Jaxb object that
         * was not used yet. */
        if (mUnusedJaxbObject &amp;&amp; mJaxbObject != null) {
            mUnusedJaxbObject = false;
            return;
        }
        mJaxbObject = mJaxbObjectFactory.create<xsl:value-of select="jaxb-type-name"/>();
    }

    /** {@inheritDoc} */
    public final void setChildrenValues() throws HostException {

         /* Make sure there is an associated JAXB object*/
        if (mJaxbObject == null) {
            createJaxbObject();
        }

        <xsl:for-each select="coxb-property[@type != 'choice']">
            <xsl:call-template name="generate-get-values-from-jaxb"/>
        </xsl:for-each>
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public final void setJaxbPropertyValue(final int index) throws HostException {
 
        ICobolBinding child = getChildrenList().get(index);
        
        /* Choice children are a special case. They directly set 
         * their parent object depending on the chosen choice
         * strategy. */
        if (child instanceof ICobolChoiceBinding) {
            return;
        }
        
        Object value = child.getObjectValue(child.getJavaType());
        if (LOG.isDebugEnabled()) {
            LOG.debug("Setting value of JAXB property "
                    + child.getJavaName()
                    + " value=" + value);
        }
        /* Set the JAXB object property value from binding object */
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:<xsl:if test="@type != 'choice'">
       <xsl:choose>
            <xsl:when test="@cobol-maxOccurs > 0 or @type = 'complexArray'">
            mJaxbObject.<xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>().clear();
            mJaxbObject.<xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>().addAll(
                (List &lt; <xsl:value-of select="@jaxb-type"/> &gt;) value);</xsl:when><xsl:otherwise>
            mJaxbObject.<xsl:value-of select="concat('set',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>((<xsl:value-of select="@jaxb-type"/>) value);</xsl:otherwise>
       </xsl:choose>
       </xsl:if>
            break;</xsl:for-each>
        default:
            break;
        }
    }
            <xsl:apply-templates select="coxb-property" mode="generate-set-jaxb-values"/>
    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
        if (type.equals(getJavaType())) {
            return mJaxbObject;
        } else {
            throw new HostException("Attempt to get binding " + getJavaName()
                    + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
    public final void setObjectValue(final Object value) throws HostException {
        if (value == null) {
            mJaxbObject = null;
            return;
        }
        if (value.getClass().equals(getJavaType())) {
            mJaxbObject = (<xsl:value-of select="jaxb-type-name"/>) value;
        } else {
            throw new HostException("Attempt to set binding " + getJavaName()
                    + " from an incompatible value " + value);
        }
    }

    /**
     * @return the java object factory for objects creation
     */
    public final ObjectFactory getObjectFactory() {
        return mJaxbObjectFactory;
    }

    /**
     * @param jaxbObjectFactory the java object factory for objects creation 
     */
    public final void setObjectFactory(final Object jaxbObjectFactory) {
        mJaxbObjectFactory = (ObjectFactory) jaxbObjectFactory;
    }

    /** {@inheritDoc} */
    public final boolean isSet() {
        return (mJaxbObject != null);
    }

    /**
     * @return the bound JAXB object
     * @throws HostException if bound JAXB object cannot be retrieved
     */
    public final <xsl:value-of select="jaxb-type-name"/> get<xsl:value-of select="jaxb-type-name"/>() throws HostException {
        return (<xsl:value-of select="jaxb-type-name"/>) getObjectValue(<xsl:value-of select="jaxb-type-name"/>.class);
    }
    
}
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements a choice binding
 -->
<xsl:template name="generate-choice-class">
<xsl:param name="binding-class-name"/>
 * Represents a choice between 2 or more elements. A choice maps to a cobol
 * REDEFINES clause exposed as an xs:choice in the corresponding XML schema
 */

public class <xsl:value-of select="$binding-class-name"/> 
             extends CChoiceBinding {

    /** Reference to a Jaxb object factory. */
    private ObjectFactory mJaxbObjectFactory;
    
    /** Alternatives of this choice binding. */
    <xsl:for-each select="coxb-property">
    /** Alternative bound to jaxb property <xsl:value-of select="@jaxb-name"/>(<xsl:value-of select="@jaxb-type"/>). */
    public <xsl:value-of select="@binding-type"/><xsl:text> </xsl:text><xsl:value-of select="@jaxb-name"/>;</xsl:for-each>

    /** Logger. */
    private static final Log LOG =
        LogFactory.getLog(<xsl:value-of select="$binding-class-name"/>.class);

    /**
     * Constructor for a Choice element.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     * @param parentBinding a reference to the parent binding
     * @param jaxbName name of field in parent JAXB object
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final ObjectFactory jaxbObjectFactory,
            final ICobolComplexBinding parentBinding,
            final String jaxbName) {
        
        super(jaxbName, null, parentBinding);
        mJaxbObjectFactory = jaxbObjectFactory;
        initAlternatives();
    }

    /** Creates a binding property for each alternative. */
    public final void initAlternatives() {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing started");
        }
        /* Create binding alternatives instances */
        <xsl:apply-templates select="coxb-property" mode="generate-property-init"/>
        /* Add alternatives to alternatives list */<xsl:for-each select="coxb-property">
        addAlternative(<xsl:value-of select="@jaxb-name"/>);</xsl:for-each>

        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing successful");
        }
    }
 
    /** {@inheritDoc} */
    public final void setAlternativesValues() throws HostException {
        Object value;
    <xsl:for-each select="coxb-property[@type != 'choice']">
        value = ((<xsl:value-of select="../jaxb-type-name"/>) getParentJaxbObject()).<xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>();
        if (value != null) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Getting value from JAXB property "
                        + "<xsl:value-of select="@jaxb-name"/>"
                        + " value=" + value);
            }
            <xsl:value-of select="@jaxb-name"/>.setObjectValue(value);
        }
    </xsl:for-each>
    }
    /** {@inheritDoc} */
    public final void setJaxbPropertyValue(
            final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        ICobolBinding alt = getAlternativesList().get(index);

        /* Choice children are a special case. They directly set 
         * their parent object depending on the chosen choice
         * strategy. */
        if (alt instanceof ICobolChoiceBinding) {
            return;
        }

        Object value = alt.getObjectValue(alt.getJavaType());
        if (LOG.isDebugEnabled()) {
            LOG.debug("Setting value of JAXB property "
                    + alt.getJavaName()
                    + " value=" + value);
        }
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:
            ((<xsl:value-of select="../jaxb-type-name"/>) getParentJaxbObject()).<xsl:value-of select="concat('set',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>(
                (<xsl:value-of select="@jaxb-type"/>) <xsl:value-of select="@jaxb-name"/>.getObjectValue(<xsl:value-of select="@jaxb-type"/>.class));
            break;</xsl:for-each>
        default:
            break;
        }
    }
    
    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
        throw new HostException("Attempt to get value from choice binding "
                + getJavaName());
    }

    /** {@inheritDoc} */
    public final void setObjectValue(final Object value) throws HostException {
        throw new HostException("Attempt to set value for choice binding "
                + getJavaName());
    }
    
    /**
     * @return the java object factory for objects creation
     */
    public final ObjectFactory getObjectFactory() {
        return mJaxbObjectFactory;
    }

    /**
     * @param jaxbObjectFactory the java object factory for objects creation 
     */
    public final void setObjectFactory(final Object jaxbObjectFactory) {
        mJaxbObjectFactory = (ObjectFactory) jaxbObjectFactory;
    }

    /** {@inheritDoc} */
    public final boolean isSet() {
        /* A Choice is considered set if at least one of its alternatives
         * is set. */
        for (ICobolBinding alt : getAlternativesList()) {
            if (alt.isSet()) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return the JAXB object bound to the parent of this choice
     * @throws HostException if parent bound JAXB object cannot be retrieved
     */
    public final <xsl:value-of select="jaxb-type-name"/> get<xsl:value-of select="jaxb-type-name"/>() throws HostException {
        return (<xsl:value-of select="jaxb-type-name"/>) getParentJaxbObject();
    }

}
</xsl:template>

<!-- ===============================================================================================
   Generate the code of the java class that implements a complex array binding
 -->
<xsl:template name="generate-complex-array-class">
<xsl:param name="binding-class-name"/>
<xsl:variable name="jaxbobject-class"><xsl:value-of select="concat('List &lt; ',jaxb-type-name,' &gt;')"/></xsl:variable>
 * Represents an array of complex (record) elements. A complex array maps to
 * a cobol OCCURS of group items and to java Lists.
 */

public class <xsl:value-of select="$binding-class-name"/> 
             extends CArrayComplexBinding {

    /** Reference to a Jaxb object factory. */
    private ObjectFactory mJaxbObjectFactory;

    /** Java object to which this cobol complex array element is bound. */
    private <xsl:value-of select="$jaxbobject-class"/> mJaxbObject;
    
    /** Logger. */
    private static final Log LOG
        = LogFactory.getLog(<xsl:value-of select="$binding-class-name"/>.class);

    /**
     * Constructor for an array of Complex elements.
     * 
     * @param jaxbObjectFactory the JAXB object factory
     * @param parentBinding a reference to the parent binding
     * @param jaxbName name of field in parent JAXB object
     * @param complexItemBinding a binding element for array items
     */
    public <xsl:value-of select="$binding-class-name"/>(
            final ObjectFactory jaxbObjectFactory,
            final ICobolComplexBinding parentBinding,
            final String jaxbName,
            final ICobolComplexBinding complexItemBinding) {
        
		super(jaxbName, <xsl:value-of select="jaxb-type-name"/>.class, parentBinding, complexItemBinding);
        mJaxbObjectFactory = jaxbObjectFactory;
        setMinOccurs(<xsl:value-of select="@cobol-minOccurs"/>);
        setMaxOccurs(<xsl:value-of select="@cobol-maxOccurs"/>);<xsl:if test="@cobol-dependingOn">
        setDependingOn("<xsl:value-of select="@cobol-dependingOn"/>");
        </xsl:if>
    }

    /** {@inheritDoc} */
    public final void createJaxbObject() throws HostException {
        mJaxbObject = new ArrayList &lt; <xsl:value-of select="jaxb-type-name"/> &gt;();
    }

    /** {@inheritDoc} */
    public final void setItemValue(
        final int index) throws HostException {
         /* Make sure there is an associated JAXB object*/
        if (mJaxbObject == null) {
            createJaxbObject();
        }
        /* Get JAXB property lsRequest */
        if (LOG.isDebugEnabled()) {
            LOG.debug("Getting value from item " +  index
                    + " of JAXB property "
                    + "<xsl:value-of select="$jaxbobject-class"/>"
                    + " value=" + mJaxbObject.get(index));
        }
        getComplexItemBinding().setObjectValue(mJaxbObject.get(index));
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public final void addJaxbPropertyValue(
        final int index) throws HostException {
         /* Make sure there is an associated JAXB object*/
        if (mJaxbObject == null) {
            throw new HostException(
                    "Binded object not initialized for " + getJavaName());
        }
        mJaxbObject.add((<xsl:value-of select="jaxb-type-name"/>) getComplexItemBinding().
        		getObjectValue(<xsl:value-of select="jaxb-type-name"/>.class));
    }
 
    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
        if (type.equals(<xsl:value-of select="jaxb-type-name"/>.class)) {
            return mJaxbObject;
        } else {
            throw new HostException("Attempt to get binding " + getJavaName()
                    + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
     @SuppressWarnings("unchecked")
    public final void setObjectValue(final Object value) throws HostException {
        if (value == null) {
            mJaxbObject = null;
            return;
        }
        if (value instanceof List) {
            if (((List) value).size() == 0) {
                mJaxbObject = new ArrayList &lt; <xsl:value-of select="jaxb-type-name"/> &gt;();
                return;
            }
            /* We assume all items will have the same type as the first one.
             * The unchecked cast might break at runtime. */
            Object item = ((List) value).get(0);
            if (item.getClass().equals(<xsl:value-of select="jaxb-type-name"/>.class)) {
                mJaxbObject = (List &lt; <xsl:value-of select="jaxb-type-name"/> &gt;) value;
                return;
            }
        }
        throw new HostException("Attempt to set binding " + getJavaName()
                + " from an incompatible value " + value);
    }

    /**
     * @return the java object factory for objects creation
     */
    public final ObjectFactory getObjectFactory() {
        return mJaxbObjectFactory;
    }

    /**
     * @param jaxbObjectFactory the java object factory for objects creation 
     */
    public final void setObjectFactory(final Object jaxbObjectFactory) {
        mJaxbObjectFactory = (ObjectFactory) jaxbObjectFactory;
    }

    /** {@inheritDoc} */
    public final boolean isSet() {
        return (mJaxbObject != null);
    }

    /**
     * @return the bound JAXB object
     * @throws HostException if bound JAXB object cannot be retrieved
     */
    @SuppressWarnings("unchecked")
    public final List &lt; <xsl:value-of select="jaxb-type-name"/> &gt; get<xsl:value-of select="jaxb-type-name"/>() throws HostException {
        return (List &lt; <xsl:value-of select="jaxb-type-name"/> &gt;) getObjectValue(<xsl:value-of select="jaxb-type-name"/>.class);
    }
    
}
</xsl:template>

<!-- ===============================================================================================
   Generate the code to initialize individual properties
 -->
<xsl:template match="coxb-property" mode="generate-property-init">
  <xsl:choose>
    <xsl:when test="@type = 'simple'">
            <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>("<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@jaxb-type"/>.class);
        </xsl:when>
        <xsl:when test="@type = 'choice'">
            <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>(mJaxbObjectFactory, this, "<xsl:value-of select="@jaxb-name"/>");
        </xsl:when>
        <xsl:when test="@type = 'complex'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@binding-type"/>(
            mJaxbObjectFactory, <xsl:choose><xsl:when test="../@type = 'complex'">
            this,</xsl:when><xsl:when test="../@type = 'choice'">
            getParentBinding(),</xsl:when></xsl:choose>
            "<xsl:value-of select="@jaxb-name"/>");
        </xsl:when>
        <xsl:when test="@type = 'complexArray'">
        <xsl:value-of select="@jaxb-name"/> = new <xsl:value-of select="@item-binding-type"/>(
                 mJaxbObjectFactory, this, "<xsl:value-of select="@jaxb-name"/>");
        <xsl:value-of select="@jaxb-name"/>Wrapper = new <xsl:value-of select="@binding-type"/>(
                 mJaxbObjectFactory, this, "<xsl:value-of select="@jaxb-name"/>", <xsl:value-of select="@jaxb-name"/>);
        </xsl:when>
  </xsl:choose>

  <xsl:if test="@cobol-byteLength">
    <xsl:value-of select="@jaxb-name"/>.setByteLength(<xsl:value-of select="@cobol-byteLength"/>);
        </xsl:if>
  <xsl:if test="@cobol-name">
    <xsl:value-of select="@jaxb-name"/>.setCobolName("<xsl:value-of select="@cobol-name"/>");
        </xsl:if>
  <xsl:if test="@cobol-isJustifiedRight = 'true'">
    <xsl:value-of select="@jaxb-name"/>.setIsJustifiedRight(true);
        </xsl:if>
  <xsl:if test="@cobol-totalDigits > 0">
    <xsl:value-of select="@jaxb-name"/>.setTotalDigits(<xsl:value-of select="@cobol-totalDigits"/>);
        </xsl:if>
  <xsl:if test="@cobol-fractionDigits > 0">
    <xsl:value-of select="@jaxb-name"/>.setFractionDigits(<xsl:value-of select="@cobol-fractionDigits"/>);
        </xsl:if>
  <xsl:if test="@cobol-isSigned = 'true'">
    <xsl:value-of select="@jaxb-name"/>.setIsSigned(true);
        </xsl:if>
  <xsl:if test="@cobol-isSignLeading = 'true'">
    <xsl:value-of select="@jaxb-name"/>.setIsSignLeading(true);
        </xsl:if>
  <xsl:if test="@cobol-isSignSeparate = 'true'">
    <xsl:value-of select="@jaxb-name"/>.setIsSignSeparate(true);
        </xsl:if>
  <xsl:if test="@cobol-minOccurs > 0">
    <xsl:value-of select="@jaxb-name"/>.setMinOccurs(<xsl:value-of select="@cobol-minOccurs"/>);
        </xsl:if>
  <xsl:if test="@cobol-maxOccurs > 0">
    <xsl:value-of select="@jaxb-name"/>.setMaxOccurs(<xsl:value-of select="@cobol-maxOccurs"/>);
        </xsl:if>
  <xsl:if test="@cobol-redefines">
    <xsl:value-of select="@jaxb-name"/>.setRedefines("<xsl:value-of select="@cobol-redefines"/>");
        </xsl:if>
  <xsl:if test="@cobol-isODOObject = 'true'">
    <xsl:value-of select="@jaxb-name"/>.setIsODOObject(true);
        </xsl:if>
  <xsl:if test="@cobol-dependingOn">
    <xsl:value-of select="@jaxb-name"/>.setDependingOn("<xsl:value-of select="@cobol-dependingOn"/>");
        </xsl:if>
  <xsl:if test="@cobol-isCustomVariable">
    <xsl:value-of select="@jaxb-name"/>.setIsCustomVariable(<xsl:value-of select="@cobol-isCustomVariable"/>);
        </xsl:if>
  <xsl:if test="@marshalChoiceStrategyClassName">
    <xsl:value-of select="@jaxb-name"/>.setMarshalChoiceStrategyClassName("<xsl:value-of select="@marshalChoiceStrategyClassName"/>");
        </xsl:if>
  <xsl:if test="@unmarshalChoiceStrategyClassName">
    <xsl:value-of select="@jaxb-name"/>.setUnmarshalChoiceStrategyClassName("<xsl:value-of select="@unmarshalChoiceStrategyClassName"/>");
        </xsl:if>
  
</xsl:template>

<!-- ===============================================================================================
   Generate the code to set binding values from JAXB instance properties
 -->
<xsl:template name="generate-get-values-from-jaxb">
    <xsl:variable name="jaxb-getter-method">
      <xsl:value-of select="concat('get',upper-case(substring(@jaxb-name,1,1)),substring(@jaxb-name,2))"/>
    </xsl:variable>
        /* Get JAXB property <xsl:value-of select="@jaxb-name"/> */
        if (LOG.isDebugEnabled()) {
            LOG.debug("Getting value from JAXB property "
                    + "<xsl:value-of select="@jaxb-name"/><xsl:if test="@type = 'complexArray'">Wrapper</xsl:if>"
                    + " value=" + mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
        }
        <xsl:value-of select="@jaxb-name"/><xsl:if test="@type = 'complexArray'">Wrapper</xsl:if>.setObjectValue(mJaxbObject.<xsl:value-of select="$jaxb-getter-method"/>());
  
</xsl:template>

<!-- ===============================================================================================
   Generate a choice stragegy skeletton source 
 -->
<xsl:template match="coxb-type" mode="choiceStrategy">
<xsl:param name="choice-strategy-class-qual-name"/>
<xsl:param name="choice-strategy-class-exists"/>

  <!-- Determine the choice strategy java source file name and location -->
  <xsl:variable name="path" select="tokenize($choice-strategy-class-qual-name,'\.')"/>
  
  <!-- If the strategy class name is fully qualified, separate package name from class name
       otherwise, use the same binding package as caller.  -->
  <xsl:variable name="choice-strategy-class-name">
    <xsl:value-of select="$path[count($path)]"/>
  </xsl:variable>
  <xsl:variable name="choice-strategy-type-package">
    <xsl:choose>
      <xsl:when test="count($path) > 1">
        <xsl:value-of select="substring($choice-strategy-class-qual-name,1,(string-length($choice-strategy-class-qual-name) - string-length($choice-strategy-class-name) - 1))"/>
      </xsl:when>
      <xsl:otherwise>
            <xsl:value-of select="concat(jaxb-type-package,'.bind')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="choice-strategy-target-dir">
    <xsl:value-of select="translate($choice-strategy-type-package,'.','/')"/>
  </xsl:variable>
  <!-- To protect custom code, the generated skeleton takes a non java
       extension.  -->
  <xsl:variable name="choice-strategy-extension">
    <xsl:choose>
      <xsl:when test="$choice-strategy-class-exists = 'true'">java.new</xsl:when>
      <xsl:otherwise>java</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  
  
<!-- Generate the dynamically built java source file -->
<xsl:result-document href="{$choice-strategy-target-dir}/{$choice-strategy-class-name}.{$choice-strategy-extension}" method="text" omit-xml-declaration="yes" indent="yes">
package <xsl:value-of select="$choice-strategy-type-package"/>;
import java.util.Hashtable;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.host.HostException;
import <xsl:value-of select="concat(jaxb-type-package, '.', jaxb-type-name)"/>;

/** 
 * Skeleton implementation of a custom choice selection strategy. Modify this
 * code to select a suitable alternative.
 */
public class <xsl:value-of select="$choice-strategy-class-name"/> implements ICobolUnmarshalChoiceStrategy {

    /** {@inheritDoc} */
    public final ICobolBinding choose(
        final ICobolChoiceBinding choice,
        final Hashtable &lt; String, Object &gt; variablesMap,
        final CobolElementVisitor visitor) throws HostException {
        
        /* Get the parent JAXB object which properties might help select the
         * right alternative. */
        <xsl:value-of select="jaxb-type-name"/> jaxbo = (<xsl:value-of select="jaxb-type-name"/>) choice.getObjectValue(<xsl:value-of select="jaxb-type-name"/>.class);
        assert (jaxbo != null);
        
        /* Replace following code with actual logic. */
        int index = 0;
        switch (index) {<xsl:for-each select="coxb-property">
        case <xsl:value-of select="position() - 1"/>:
            return choice.getAlternativeByJavaName("<xsl:value-of select="@jaxb-name"/>");</xsl:for-each>
        case -1:
            /* An exemple of how to signal an exception.*/
            throw (new HostException("Unable to select an alternative"));
        default:
            /* Null will let the default choice strategy apply. */
            return null;
        }
    }
}
</xsl:result-document>

</xsl:template>

</xsl:stylesheet>
