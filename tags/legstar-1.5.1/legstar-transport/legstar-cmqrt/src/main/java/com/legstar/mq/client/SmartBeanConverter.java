package com.legstar.mq.client;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.osjava.sj.loader.convert.Converter;

/**
 * Basically a clone of org.osjava.sj.loader.convert.BeanConverter but with
 * minimal numeric properties handling.
 * 
 */
public class SmartBeanConverter implements Converter {

    public Object convert(Properties properties, String type) {
        String value = properties.getProperty("");

        if (value != null) {
            throw new RuntimeException(
                    "Specify the value as a pseudo property as Beans have empty constructors");
        }

        String methodName = null;

        try {
            Class < ? > c = Class.forName(type);
            Object bean = c.newInstance();
            Iterator < Object > itr = properties.keySet().iterator();
            while (itr.hasNext()) {
                String key = (String) itr.next();
                if ("converter".equals(key) || "type".equals(key)) {
                    continue;
                }
                Object property = properties.get(key);
                if (property instanceof String) {
                    String fieldName = Character.toTitleCase(key.charAt(0))
                            + key.substring(1);
                    methodName = "get" + fieldName;
                    Method m = c.getMethod(methodName);
                    Class < ? > returnType = m.getReturnType();

                    methodName = "set" + fieldName;
                    m = c.getMethod(methodName, new Class[] { returnType });
                    Object typedValue = null;
                    if (returnType == int.class) {
                        typedValue = Integer.valueOf((String) property);
                    } else {
                        typedValue = property;
                    }

                    m.invoke(bean, new Object[] { typedValue });
                } else if (property instanceof List) {
                    List < ? > list = (List < ? >) property;
                    int sz = list.size();
                    key = "add" + Character.toTitleCase(key.charAt(0))
                            + key.substring(1);
                    Method m = c.getMethod(key, new Class[] { Integer.TYPE,
                            String.class });
                    for (int i = 0; i < sz; i++) {
                        Object item = list.get(i);
                        if (item instanceof String) {
                            m.invoke(bean, new Object[] { new Integer(i),
                                    (String) item });
                        } else {
                            throw new RuntimeException(
                                    "Only Strings and Lists of String are supported");
                        }
                    }
                } else {
                    throw new RuntimeException(
                            "Only Strings and Lists of Strings are supported");
                }
            }
            return bean;
        } catch (ClassNotFoundException cnfe) {
            throw new RuntimeException("Unable to find class: " + type, cnfe);
        } catch (NoSuchMethodException nsme) {
            throw new RuntimeException("Unable to find method " + methodName
                    + " on class: " + type, nsme);
        } catch (InstantiationException ie) {
            throw new RuntimeException("Unable to instantiate class: " + type,
                    ie);
        } catch (IllegalAccessException ie) {
            throw new RuntimeException("Unable to access class: " + type, ie);
        } catch (IllegalArgumentException iae) {
            throw new RuntimeException("Unable to pass argument to class: "
                    + type, iae);
        } catch (InvocationTargetException ite) {
            throw new RuntimeException(
                    "Unable to invoke (String) constructor on class: " + type,
                    ite);
        }

    }

}
