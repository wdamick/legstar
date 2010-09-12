package com.legstar.coxb.util;

import net.sf.antcontrib.property.AbstractPropertySetterTask;

import org.apache.tools.ant.BuildException;

/**
 * A simple ant task to change a property casing.
 * 
 */
public class PropertyCase extends AbstractPropertySetterTask {

    /** Source property. */
    private String _from;

    /** To uppercase if true otherwise to lowercase. */
    private boolean _toUpper = true;

    /**
     * @param name the new property to receive cased value
     */
    public void setName(final String name) {
        setProperty(name);
    }

    /**
     * @param from the source property
     */
    public void setFrom(final String from) {
        _from = from;
    }

    /**
     * @param toUpper to uppercase if true, to lowercase otherwise
     */
    public void setToupper(final boolean toUpper) {
        _toUpper = toUpper;
    }

    /**
     * {@inheritDoc}
     * 
     * @see net.sf.antcontrib.property.AbstractPropertySetterTask#validate()
     */
    protected void validate() {
        super.validate();
        if (_from == null) {
            throw new BuildException("Missing the 'from' attribute.");
        }
    }

    /**
     * {@inheritDoc}
     */
    public void execute() {
        validate();

        String value = getProject().getProperty(_from);

        if (value == null) {
            throw new BuildException("Property '" + _from + "' is not defined.");
        }

        setPropertyValue((_toUpper) ? value.toUpperCase() : value.toLowerCase());
    }
}
