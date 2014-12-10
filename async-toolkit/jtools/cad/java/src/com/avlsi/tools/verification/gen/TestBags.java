package com.avlsi.tools.verification.gen;

public class TestBags {

    /**  TestBags executes all the different functions that can be
     * performed on/by GrabBags
     *
     **/
        
    public static void main(String[] args) {

        byte[] array1 = {1, 1, 1};
        byte[] array2 = {2, 2, 2};
        byte[] array3 = {3, 3, 3};
        byte[] array4 = {4, 4, 4};
        byte[] array5 = {5, 5, 5};

        Packet p1 = new Packet(array1);
        Packet p2 = new Packet(array2);
        Packet p3 = new Packet(array3);
        Packet p4 = new Packet(array4);
        Packet p5 = new Packet(array5);

        OrderedGrabBag g1 = new OrderedGrabBag();

        g1.add(p5);
        g1.add(p4);
        g1.add(p3);
        g1.add(p2);
        g1.add(p1);

        Distribution d1 = new Distribution(0);
        
        Streamable next = g1.getNext();
        while(next != null) {
            d1.add(next);
            next = g1.getNext();
        }
        g1.refresh();
        next = g1.getNext();
        while(next != null) {
            d1.add(next);
            next = g1.getNext();
        }

        generatePrint(d1,"d1");

        RandomGrabBag g2 = new RandomGrabBag();
        
        g2.add(p1);
        g2.add(p2);
        g2.add(p3);

        Distribution d2 = new Distribution(0);

        for(int i=0; i<10; i++) {
            d2.add(g2.getNext());
        }

        generatePrint(d2,"d2");

        RandomGrabBag g3 = new RandomGrabBag(true,true,100);

        g3.add(p1);
        g3.add(p2,400);
        g3.add(p3,500);
        
        Distribution d3 = new Distribution(0);

        for(int i=0; i<10; i++) {
            d3.add(g3.getNext());
        }

        generatePrint(d3,"d3");
                                        
        RandomGrabBag g4 = new RandomGrabBag(false,false);

        g4.add(p1);
        g4.add(p2);
        g4.add(p3);
        g4.add(p4);
        g4.add(p5);

        Distribution d4 = new Distribution(0);
        
        for(int i=0; i<10; i++) {
            next = g4.getNext();
            if (next == null) {
                g4.refresh();
                next = g4.getNext();
            }
            d4.add(next);
        }

        generatePrint(d4,"d4");

        RandomGrabBag g5 = new RandomGrabBag(true,false);

        g5.add(p1);
        g5.add(p2, 2*g5.getDefaultWeight());
        g5.add(p3, 3*g5.getDefaultWeight());

        Distribution d5 = new Distribution(0);

        for(int i=0; i<10; i++) {
            next = g5.getNext();
            if (next == null) {
                g5.refresh();
                next = g5.getNext();
            }
            d5.add(next);
        }

        generatePrint(d5,"d5");

        RandomGrabBag g6 = new RandomGrabBag(50);

        g6.add(p1);
        g6.add(p2);
        g6.setDefaultWeight(100);
        g6.add(p3);
        g6.add(p4);

        Distribution d6 = new Distribution(0);

        for(int i=0; i<10; i++) {
            next = g6.getNext();
            if (next == null) {
                g6.refresh();
                next = g6.getNext();
            }
            d6.add(next);
        }

        generatePrint(d6,"d6");
        
    }

    public static void generatePrint(Streamable s, String name) {
        byte[] output = new byte[100];
        int offset = 0;
        int length = 100;

        int added = s.generate(output,offset,length);
        offset += added;
        length -= added;
        System.out.print(name+" : ");
        for(int i=0; i<offset; i++) {
            System.out.print(output[i]+" : ");
        }
        System.out.println(" end of "+name);
    }

}
