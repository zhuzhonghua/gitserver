package org.scm;

import java.io.IOException;
import clojure.lang.Compiler.CompilerException;
import clojure.lang.RT;
import clojure.lang.Var;

public class CloUtil {

    /**
     * @param file
     */
    public static void load(String file) {
        System.out.println("[load]"+file);
        try {
            RT.loadResourceScript(file);
        } catch (CompilerException ce) {
            System.err.println(ce.getCause());
            System.err.println(ce);
        } catch (IOException ie) {
            System.err.println(ie);
        }
    }

    /**
     * @param expr
     */
    public static void eval(String expr) {
        System.out.println("[eval]"+expr);
        try {
            Var eval = RT.var("clojure.core", "eval");
            Object result = eval.invoke(RT.readString(expr));
            if (result != null) {
                System.out.println(result);
            }
        } catch (CompilerException ce) {
            System.err.println(ce);
        }
    }

        /**
         * @param ns
         * @param func
         * @param params
         */
    public static void call(String ns, String func, Object... params) {
        System.out.println("[call]"+ns+" "+func+" "+params);
        Var call = RT.var(ns, func);
        call.applyTo(RT.seq(params));
    }
}
