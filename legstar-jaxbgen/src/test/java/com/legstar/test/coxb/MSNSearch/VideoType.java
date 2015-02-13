
package com.legstar.test.coxb.MSNSearch;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Video complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Video">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="PlayUrl" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="SourceTitle" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="Format" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="RunTime" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *         &lt;element name="Width" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *         &lt;element name="Height" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *         &lt;element name="FileSize" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *         &lt;element name="StaticThumbnail" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}StaticThumbnail" minOccurs="0"/>
 *         &lt;element name="MotionThumbnail" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}MotionThumbnail" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Video", propOrder = {
    "playUrl",
    "sourceTitle",
    "format",
    "runTime",
    "width",
    "height",
    "fileSize",
    "staticThumbnail",
    "motionThumbnail"
})
public class VideoType
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "PlayUrl")
    @CobolElement(cobolName = "PlayUrl", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(32)", usage = "DISPLAY")
    protected String playUrl;
    @XmlElement(name = "SourceTitle")
    @CobolElement(cobolName = "SourceTitle", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(32)", usage = "DISPLAY")
    protected String sourceTitle;
    @XmlElement(name = "Format")
    @CobolElement(cobolName = "Format", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(32)", usage = "DISPLAY")
    protected String format;
    @XmlElement(name = "RunTime")
    @CobolElement(cobolName = "RunTime", type = CobolType.BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "9(9)", usage = "COMP-5")
    protected Integer runTime;
    @XmlElement(name = "Width")
    @CobolElement(cobolName = "Width", type = CobolType.BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "9(9)", usage = "COMP-5")
    protected Integer width;
    @XmlElement(name = "Height")
    @CobolElement(cobolName = "Height", type = CobolType.BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "9(9)", usage = "COMP-5")
    protected Integer height;
    @XmlElement(name = "FileSize")
    @CobolElement(cobolName = "FileSize", type = CobolType.BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "9(9)", usage = "COMP-5")
    protected Integer fileSize;
    @XmlElement(name = "StaticThumbnail")
    @CobolElement(cobolName = "StaticThumbnail", type = CobolType.GROUP_ITEM, levelNumber = 15)
    protected StaticThumbnailType staticThumbnail;
    @XmlElement(name = "MotionThumbnail")
    @CobolElement(cobolName = "MotionThumbnail", type = CobolType.GROUP_ITEM, levelNumber = 15)
    protected MotionThumbnailType motionThumbnail;

    /**
     * Gets the value of the playUrl property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPlayUrl() {
        return playUrl;
    }

    /**
     * Sets the value of the playUrl property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPlayUrl(String value) {
        this.playUrl = value;
    }

    public boolean isSetPlayUrl() {
        return (this.playUrl!= null);
    }

    /**
     * Gets the value of the sourceTitle property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSourceTitle() {
        return sourceTitle;
    }

    /**
     * Sets the value of the sourceTitle property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSourceTitle(String value) {
        this.sourceTitle = value;
    }

    public boolean isSetSourceTitle() {
        return (this.sourceTitle!= null);
    }

    /**
     * Gets the value of the format property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFormat() {
        return format;
    }

    /**
     * Sets the value of the format property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFormat(String value) {
        this.format = value;
    }

    public boolean isSetFormat() {
        return (this.format!= null);
    }

    /**
     * Gets the value of the runTime property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getRunTime() {
        return runTime;
    }

    /**
     * Sets the value of the runTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setRunTime(Integer value) {
        this.runTime = value;
    }

    public boolean isSetRunTime() {
        return (this.runTime!= null);
    }

    /**
     * Gets the value of the width property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getWidth() {
        return width;
    }

    /**
     * Sets the value of the width property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setWidth(Integer value) {
        this.width = value;
    }

    public boolean isSetWidth() {
        return (this.width!= null);
    }

    /**
     * Gets the value of the height property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getHeight() {
        return height;
    }

    /**
     * Sets the value of the height property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setHeight(Integer value) {
        this.height = value;
    }

    public boolean isSetHeight() {
        return (this.height!= null);
    }

    /**
     * Gets the value of the fileSize property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getFileSize() {
        return fileSize;
    }

    /**
     * Sets the value of the fileSize property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setFileSize(Integer value) {
        this.fileSize = value;
    }

    public boolean isSetFileSize() {
        return (this.fileSize!= null);
    }

    /**
     * Gets the value of the staticThumbnail property.
     * 
     * @return
     *     possible object is
     *     {@link StaticThumbnailType }
     *     
     */
    public StaticThumbnailType getStaticThumbnail() {
        return staticThumbnail;
    }

    /**
     * Sets the value of the staticThumbnail property.
     * 
     * @param value
     *     allowed object is
     *     {@link StaticThumbnailType }
     *     
     */
    public void setStaticThumbnail(StaticThumbnailType value) {
        this.staticThumbnail = value;
    }

    public boolean isSetStaticThumbnail() {
        return (this.staticThumbnail!= null);
    }

    /**
     * Gets the value of the motionThumbnail property.
     * 
     * @return
     *     possible object is
     *     {@link MotionThumbnailType }
     *     
     */
    public MotionThumbnailType getMotionThumbnail() {
        return motionThumbnail;
    }

    /**
     * Sets the value of the motionThumbnail property.
     * 
     * @param value
     *     allowed object is
     *     {@link MotionThumbnailType }
     *     
     */
    public void setMotionThumbnail(MotionThumbnailType value) {
        this.motionThumbnail = value;
    }

    public boolean isSetMotionThumbnail() {
        return (this.motionThumbnail!= null);
    }

}
