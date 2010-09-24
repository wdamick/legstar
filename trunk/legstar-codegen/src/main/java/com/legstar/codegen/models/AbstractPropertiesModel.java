package com.legstar.codegen.models;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Simple models with ability to serialize, deserialize from properties files.
 * 
 */
public abstract class AbstractPropertiesModel {

    /**
     * A no-Arg constructor.
     */
    public AbstractPropertiesModel() {

    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public AbstractPropertiesModel(final Properties props) {

    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public abstract Properties toProperties();

    /**
     * Gets a boolean property from a properties file.
     * 
     * @param props the property file
     * @param key the item key
     * @param defaultValue the default value if key not found
     * @return either the value or the default if key not found
     */
    public boolean getBoolean(final Properties props,
            final String key, final boolean defaultValue) {
        String value = props.getProperty(key);
        if (value == null) {
            return defaultValue;
        } else {
            return Boolean.valueOf(value);
        }
    }

    /**
     * Puts a boolean value in a property file if the value is not null.
     * 
     * @param props the properties file
     * @param key the item key
     * @param value the item value
     */
    public void putBoolean(final Properties props, final String key,
            final Boolean value) {
        if (value != null) {
            props.put(key, value.toString());
        }
    }

    /**
     * Gets a long property from a properties file.
     * 
     * @param props the property file
     * @param key the item key
     * @param defaultValue the default value if key not found
     * @return either the value or the default if key not found
     */
    public long getLong(final Properties props,
            final String key, final long defaultValue) {
        String value = props.getProperty(key);
        if (value == null) {
            return defaultValue;
        } else {
            return Long.parseLong(value);
        }
    }

    /**
     * Puts a long value in a property file if the value is not null.
     * 
     * @param props the properties file
     * @param key the item key
     * @param value the item value
     */
    public void putLong(final Properties props, final String key,
            final Long value) {
        if (value != null) {
            props.put(key, value.toString());
        }
    }

    /**
     * Gets a string property from a properties file.
     * 
     * @param props the property file
     * @param key the item key
     * @param defaultValue the default value if key not found
     * @return either the value or the default if key not found
     */
    public String getString(final Properties props,
            final String key, final String defaultValue) {
        String value = props.getProperty(key);
        if (value == null) {
            return defaultValue;
        } else {
            return value;
        }
    }

    /**
     * Puts a string value in a property file if the value is not null.
     * 
     * @param props the properties file
     * @param key the item key
     * @param value the item value
     */
    public void putString(final Properties props, final String key,
            final String value) {
        if (value != null) {
            props.put(key, value);
        }
    }

    /**
     * Gets a file property from a properties file.
     * 
     * @param props the property file
     * @param key the item key
     * @param defaultValue the default value if key not found
     * @return either the value or the default if key not found
     */
    public File getFile(final Properties props,
            final String key, final File defaultValue) {
        String pathName = getString(props, key, null);
        if (pathName == null) {
            return defaultValue;
        } else {
            return new File(pathName);
        }
    }

    /**
     * Puts a File path name in a property file if the value is not null.
     * 
     * @param props the properties file
     * @param key the item key
     * @param value the item value
     */
    public void putFile(final Properties props, final String key,
            final File value) {
        if (value != null) {
            try {
                props.put(key, value.getCanonicalPath());
            } catch (IOException e) {
                props.put(key, e.getMessage());
            }
        }
    }

    /**
     * Gets a string list property from a properties file.
     * 
     * @param props the property file
     * @param key the item key
     * @param defaultValue the default value if key not found
     * @return either the value or the default if key not found
     */
    public List < String > getStringList(final Properties props,
            final String key, final List < String > defaultValue) {
        int i = 0;
        String value = getString(props, key + '_' + Integer.toString(i), null);
        if (value == null) {
            return defaultValue;
        }
        List < String > list = new ArrayList < String >();
        while (value != null) {
            list.add(value);
            i++;
            value = getString(props, key + '_' + Integer.toString(i), null);
        }
        return list;
    }

    /**
     * Puts a String list in a property file if the value is not null.
     * 
     * @param props the properties file
     * @param key the item key
     * @param value the item value
     */
    public void putStringList(final Properties props, final String key,
            final List < String > value) {
        if (value != null) {
            int i = 0;
            for (String s : value) {
                putString(props, key + '_' + Integer.toString(i), s);
                i++;
            }
        }
    }

    /**
     * Gets a URI property from a properties file.
     * 
     * @param props the property file
     * @param key the item key
     * @param defaultValue the default value if key not found
     * @return either the value or the default if key not found
     */
    public URI getURI(final Properties props,
            final String key, final URI defaultValue) {
        String value = getString(props, key, null);
        if (value == null) {
            return defaultValue;
        }
        try {
            return new URI(value);
        } catch (URISyntaxException e) {
            return null;
        }
    }

    /**
     * Puts a URI in a property file if the value is not null.
     * 
     * @param props the properties file
     * @param key the item key
     * @param value the item value
     */
    public void putURI(final Properties props, final String key,
            final URI value) {
        if (value != null) {
            putString(props, key, value.toString());
        }
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toProperties().toString();
    }

}
