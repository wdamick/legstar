package com.legstar.proxy.invoke;

import java.util.Map;

import com.legstar.coxb.transform.IHostTransformers;

/**
 * Generic code for proxy operations.
 *
 */
public interface IOperationProxy {

    /** Configuration property giving the host character set. */ 
    String HOST_CHARSET_PROPERTY = "hostCharset";
    
    /**
     * Invoke a remote operation.
     * <p/>
     * @param requestID a unique identifier for this request
     * @param requestBytes the request bytes payload 
     * @return the reply java data object
     * @throws ProxyInvokerException if invoke fails
     */
      byte[] invoke(
            final String requestID,
            final byte[] requestBytes) throws ProxyInvokerException;
    
      /**
       * Invoke a remote operation.
       * <p/>
       * @param config runtime configuration parameters
       * @param requestID a unique identifier for this request
       * @param requestBytes the request bytes payload 
       * @return the reply java data object
       * @throws ProxyInvokerException if invoke fails
       */
        byte[] invoke(
              final Map < String, String > config,
              final String requestID,
              final byte[] requestBytes) throws ProxyInvokerException;

        /**
       * @return the host transformers for request java data object
       */
      IHostTransformers getRequestTransformers();

      /**
       * @param requestTransformers the host transformers for request java data object to set
       */
      void setRequestTransformers(
              final IHostTransformers requestTransformers);

      /**
       * @return the host transformers for response java data object
       */
      IHostTransformers getResponseTransformers();

      /**
       * @param responseTransformers the host transformers for response java data object to set
       */
      void setResponseTransformers(
              final IHostTransformers responseTransformers);
      
    /**
     * Return a proxy invoker capable of calling the target process.
     * @param config a set of configuration parameters
     * @return a proxy invoker
     * @throws ProxyInvokerException if proxy invoker cannot be created
     */
    IProxyInvoker getProxyInvoker(
            final Map < String, String > config) throws ProxyInvokerException;

    /**
     * @param config the current configuration set
     * @return the host character set to use
     */
    String getHostCharset(final Map < String, String > config);

    /**
     * @return the current proxy invoker
     */
    IProxyInvoker getProxyInvoker();

    /**
     * @param proxyInvoker the proxy invoker to set
     */
    void setProxyInvoker(final IProxyInvoker proxyInvoker);
    
    /**
     * @return the current configuration parameter set
     */
    Map < String, String > getConfig();
    
    /**
     * @param config the current configuration parameter set to set
     */
    void setConfig(final Map < String, String > config);
}
