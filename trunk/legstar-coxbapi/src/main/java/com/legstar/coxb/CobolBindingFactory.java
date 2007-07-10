package com.legstar.coxb;

/**
 * Instanciate a concrete binding factory.
 */
public final class CobolBindingFactory {
	
	/** In this version the factory name is hardcoded. In a future release,
	 * this will be pulled from some configuration file. */
	private static final String FACTORY_NAME =
		"com.legstar.coxb.impl.CBindingFactory";
	
	/** Private constructor to prevent instanciation of this final class. */
	private CobolBindingFactory() {
	}
	
	/**
	 * Create a concrete binding factory.
	 * @return a binding factory ready to create binding elements
	 */
	public static ICobolBindingFactory getBindingFactory() {
		try {
			Class ofClass = Class.forName(FACTORY_NAME);
			Object of = ofClass.newInstance();
			return (ICobolBindingFactory) of;
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

}
