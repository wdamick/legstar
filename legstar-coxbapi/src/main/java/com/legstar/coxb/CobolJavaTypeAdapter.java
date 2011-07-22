package com.legstar.coxb;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Retention(RUNTIME)
@Target({ FIELD })
public @interface CobolJavaTypeAdapter {

    /**
     * Points to the class that converts a COBOL type to a java type or vice
     * versa. See {@link ICobolBinding} for more details.
     */
    Class < ? extends ICobolBinding > value();

}
