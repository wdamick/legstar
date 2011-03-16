package com.legstar.cobc.gen;

import junit.framework.TestCase;

/**
 * Test CobolNameResolver.
 * 
 */
public class CobolNameResolverTest extends TestCase {

    /** Name resolver instance. */
    private CobolNameResolver _nameResolver;

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        _nameResolver = new CobolNameResolver();
    }

    /**
     * Test a non reserved COBOL name which is unique.
     * 
     * @throws Exception
     */
    public void testNonReservedUnique() throws Exception {
        assertEquals("AB-cd586", _nameResolver.getUniqueName("AB_cd@586"));
    }

    /**
     * Test a with name conflict.
     * 
     * @throws Exception
     */
    public void testNameConflict() throws Exception {
        assertEquals("AB-cd", _nameResolver.getUniqueName("9AB_cd_"));
        assertEquals("AB-cd0", _nameResolver.getUniqueName("9AB_cd_"));
        assertEquals("AB-cd1", _nameResolver.getUniqueName("9AB_cd_"));
    }

    /**
     * Test reserved words.
     * 
     * @throws Exception
     */
    public void testReservedWords() throws Exception {
        assertEquals("R-ALPHANUMERIC",
                _nameResolver.getUniqueName("ALPHANUMERIC"));
        assertEquals("R-ALPHANUMERIC0",
                _nameResolver.getUniqueName("ALPHANUMERIC"));
        assertEquals("R-COMP-5", _nameResolver.getUniqueName("COMP-5"));
        assertEquals("R-EVALUATE", _nameResolver.getUniqueName("EVALUATE"));
    }
}
