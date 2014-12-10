/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.recalc;


import com.avlsi.test.AbstractTestCase;

/**
 * Class that tests recalc engine
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class TestRecalc extends AbstractTestCase {
    public void test() throws Exception {
        // Case 1: Test get and set of independant variables
        IndependantVariable i = new IndependantVariable(0);
        i.set(10);
        assertTrue(i.get() == 10);
        i.set(20);
        assertTrue(i.get() == 20);

        // Case 2: Test dependant variable evaluation
        DependantVariable x = new DependantVariable(i);
        i.set(10);
        assertTrue(x.get() == 10);
        i.set(20);
        assertTrue(x.get() == 20);

        // Case 3: Plus Operand
        x = new DependantVariable(new PlusOp(new Literal(5), i));
        i.set(10);
        assertTrue(x.get() == 5+10);
        i.set(20);
        assertTrue(x.get() == 5+20);

        // Case 4: Minus Operand
        x = new DependantVariable(new MinusOp(new Literal(5), i));
        i.set(10);
        assertTrue(x.get() == 5-10);
        i.set(20);
        assertTrue(x.get() == 5-20);

        // Case 5: Multiplication Operand
        x = new DependantVariable(new MultOp(i, i));
        i.set(10);
        assertTrue(x.get() == 10*10);
        i.set(20);
        assertTrue(x.get() == 20*20);

        // Case 6: Division Operand
        x = new DependantVariable(new DivOp(new MultOp(new Literal(2), i), i));
        i.set(10);
        assertTrue(x.get() == 2*10/10);
        i.set(20);
        assertTrue(x.get() == 2*20/20);

        // Case 7: Modulo Operand
        x = new DependantVariable(new ModOp(i, new Literal(7)));
        i.set(10);
        assertTrue(x.get() == 10%7);
        i.set(-10);
        assertTrue(x.get() == -10%7);

        // Case 8: Power Operand
        x = new DependantVariable(new PowOp(i, new Literal(3)));
        i.set(10);
        assertTrue(x.get() == Math.pow(10,3));
        i.set(-10);
        assertTrue(x.get() == Math.pow(-10,3));

        // Case 9: Negation Operand
        x = new DependantVariable(new NegateOp(i));
        i.set(10);
        assertTrue(x.get() == -10);
        i.set(-10);
        assertTrue(x.get() == -(-10));

        // Case 10: Multiple dependancies
        x = new DependantVariable(new NegateOp(i));
        DependantVariable y = new DependantVariable(new MultOp(x,i));
        i.set(10);
        assertTrue(y.get() == -10*10);
        i.set(-10);
        assertTrue(y.get() == -(-10)*-10);

        // Case 11: Division by zero
        DivOp op = new DivOp(new Literal(0), new Literal(0));
        assertTrue(Double.isNaN(op.eval()));

        // Case 12: Arbitrary static function call
        try {
            Function f = new Function("Math", "sin",
                                      new ExpressionInterface[] {i});
            i.set(Math.PI/2);
            assertTrue(f.eval() == Math.sin(Math.PI/2));
        } catch (NoSuchMethodException e) {
            System.out.println(e);
            assertTrue(false);
        } catch (ClassNotFoundException e) {
            System.out.println(e);
            assertTrue(false);
        }

/*
        // Case 13: Arbitrary dynamic function call
        try {
            Double d = new Double(10);
            Function f = new Function(x, "doubleValue", null);
            assertTrue(f.eval() == 10);
        } catch (NoSuchMethodException e) {
            System.out.println(e);
            assertTrue(false);
        }
*/
    }

    public static void main (String[] args) {
        AbstractTestCase.testOne(new TestRecalc());
    }
}
