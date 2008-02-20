package com.legstar.cixs.jaxws.gen;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.coxb.host.HostException;
import com.legstar.util.JaxbUtil;

/**
 * Provides the generator with convenience methods. The class can be 
 * passed as an instance to the velocity engine and used by templates.
 * 
 */
public class CixsHelper {
	
	/**
	 * Optionally an operation can have its own package name, different
	 * from the service. This makes sure we always get a valid package
	 * name.
	 * @param operation operation
	 * @param defaultPackageName to use if operation doesn't have a package
	 * of its own
	 * @return a valid package name
	 */
	public final String getOperationPackageName(
			final CixsOperation operation,
			final String defaultPackageName) {
		if (operation.getPackageName() != null
				&& operation.getPackageName().length() > 0) {
			return operation.getPackageName();
		} else {
			return defaultPackageName;
		}
	}
	
	/**
	 * Optionally an operation can have its own namespace name, different
	 * from the service. This makes sure we always get a valid namespace
	 * name.
	 * @param operation operation
	 * @param defaultNamespace to use if operation doesn't have a namespace
	 * of its own
	 * @return a valid namespace name
	 */
	public final String getOperationNamespace(
			final CixsOperation operation,
			final String defaultNamespace) {
		if (operation.getNamespace() != null
				&& operation.getNamespace().length() > 0) {
			return operation.getNamespace();
		} else {
			return defaultNamespace;
		}
	}
	
	/**
	 * A mere wrapper on the static <code>JaxbUtil.byteLength</code>.
	 * TODO revise JaxbUtil to get a numeric rather than a string
	 * @param jaxbPackage the java package name from which an ObjectFactory
	 *        can be instanciated
	 * @param jaxbTypeName the JAXB type name of the object for which byte 
	 *        length must be returned
	 * @return the byte length as a string
	 * @throws HostException if byte length calculation failed
	 */
	public final long getByteLength(
			final String jaxbPackage,
			final String jaxbTypeName) throws HostException {
		return Long.parseLong(JaxbUtil.byteLength(jaxbPackage, jaxbTypeName));
	}
	
}
