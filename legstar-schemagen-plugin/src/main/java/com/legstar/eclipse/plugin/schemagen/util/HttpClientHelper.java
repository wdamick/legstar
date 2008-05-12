package com.legstar.eclipse.plugin.schemagen.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.GetMethod;

/**
 * This is a simple usage of the Apache Http client.
 */
public class HttpClientHelper {
    
    /** An apache http client. */
	private HttpClient mHttpClient;
    
    /** Contructs the helper. */
	public HttpClientHelper() {
        mHttpClient = new HttpClient();
    }
    
    /**
     * Issue an Http get. If a user/password is provided,  send them as basic
     * authentication.
     * @param strUrl the target URL as a string
     * @param user the basic authentication user
     * @param password the basic authentication password
     * @return a String that is the content of the reply
     * @throws HttpClientHelperException if the get fails
     */
    public String get(
            final String strUrl,
            final String user,
            final String password) throws HttpClientHelperException {
        BufferedReader in = null;
        try {
            StringBuilder sb = new StringBuilder();
            HttpMethod httpMethod = new GetMethod(strUrl);
            if (user != null) {
                httpMethod.setDoAuthentication(true);
                HttpState state = new HttpState();
                state.setCredentials(new AuthScope(null, -1, null, null),
                        new UsernamePasswordCredentials(user, password));
                mHttpClient.setState(state);
            }
            mHttpClient.executeMethod(httpMethod);
            if (httpMethod.getStatusCode() > 200) {
                throw new HttpClientHelperException(
                        "HTTP error " + httpMethod.getStatusCode()
                        + " " + httpMethod.getStatusText());
            }
            /* This is where the client blocks waiting for a reply. */
            in = new BufferedReader(
                    new InputStreamReader(
                            httpMethod.getResponseBodyAsStream()));
            String inputLine;
            while ((inputLine = in.readLine()) != null) {
                sb.append(inputLine);
            }
            return sb.toString();
        } catch (IOException e) {
            throw new HttpClientHelperException(e);
        } catch (IllegalStateException e) {
            throw new HttpClientHelperException(e);
        } catch (IllegalArgumentException e) {
            throw new HttpClientHelperException(e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                	return null;
                }
            }
        }
    }

}
