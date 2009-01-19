package com.legstar.proxy.invoke;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import com.legstar.coxb.transform.IHostTransformers;
import com.legstar.coxb.util.Utils;

/**
 * An operation proxy that directly works with generated transformers.
 * <p/>
 * This is a high performance alternative to <code>ReflectOperationProxy</code>.
 * <p/>
 * Transformers must have been generated using the COBOL binding generator
 * and the generated classes must be available from the classpath.
 *
 */
public class DirectOperationProxy extends AbstractOperationProxy {

    /** Request transformers class name. */ 
    public static final String REQUEST_TRANSFORMERS_CLASS_NAME_PROPERTY =
        "requestTransformersClassName";

    /** Response transformers class name. */ 
    public static final String RESPONSE_TRANSFORMERS_CLASS_NAME_PROPERTY =
        "responseTransformersClassName";

    /**
     * Create the proxy invoker for the set of configuration parameters passed.
     * The configuration parameters expected are:
     * <ul>
     *  <li>requestTransformersClassName: Request transformers class name</li>
     *  <li>responseTransformersClassName: Response transformers class name</li>
     * </ul>
     * @param config the initial set of parameters
     * @throws ProxyConfigurationException if transformers cannot be located from
     *  the classpath
     */
    public DirectOperationProxy(
            final Map < String, String > config) throws ProxyConfigurationException {
        super(config);
        setRequestTransformers(getHostTransformers(
                getConfigParm(REQUEST_TRANSFORMERS_CLASS_NAME_PROPERTY)));
        setResponseTransformers(getHostTransformers(
                getConfigParm(RESPONSE_TRANSFORMERS_CLASS_NAME_PROPERTY)));
    }
    
    /**
     * Load host transformers identified by class name.
     * @param className the transformers class name
     * @return an instance of the host transformers
     * @throws ProxyConfigurationException if unable to instantiate the host transformers
     */
    private IHostTransformers getHostTransformers(
            final String className)  throws ProxyConfigurationException {
        try {
            Class < ? > clazz = Utils.loadClass(className);
            Constructor < ? > constructor = clazz.getConstructor();
            return (IHostTransformers) constructor.newInstance(new Object[] {});
        } catch (SecurityException e) {
            throw new ProxyConfigurationException(e);
        } catch (IllegalArgumentException e) {
            throw new ProxyConfigurationException(e);
        } catch (ClassNotFoundException e) {
            throw new ProxyConfigurationException(e);
        } catch (NoSuchMethodException e) {
            throw new ProxyConfigurationException(e);
        } catch (InstantiationException e) {
            throw new ProxyConfigurationException(e);
        } catch (IllegalAccessException e) {
            throw new ProxyConfigurationException(e);
        } catch (InvocationTargetException e) {
            throw new ProxyConfigurationException(e.getTargetException());
        }
    }

}