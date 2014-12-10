package com.avlsi.tools.verification.gen;

import com.avlsi.tools.verification.gen.*;
import java.io.*;

public class TestGen {

    public static int totalLength;
    
    /** A test of all the stimulus generation Packet and Distribution types. 
     * Should be called with one argument representing the length of all the Packets and 
     * Distributions to be created.  One of these packets will read the file test.in, 
     * if this file does not exist, it will raise a FileNotFoundException.
     *
     **/
    
    public static void main(String[] args) {
        try
        {
            totalLength = Integer.parseInt(args[0]);
        
            byte[] arrayin = {23, 99, -43, 24, 85};
            Packet pArray = new Packet(arrayin);

            Packet pString = new Packet("HELLO");
    
            Packet pR1 = new RandomPacket(10);
            Packet pR2 = new RandomPacket(10,(byte) 20,(byte) 29);
            Packet pLFSR1 = new LFSRPacket(10,30);
            Packet pLFSR2 = new LFSRPacket(10,18,(byte) 15,(byte) 20);
            Packet pLFSR3 = new LFSRPacket(10,43,(byte) 40,(byte) 49);

            File iFile = new File("test.in");
            FilePacket pFile = new FilePacket(iFile);
        

            Distribution d1 = new Distribution(0);
            d1.add(pArray);
            d1.add(pString);
            d1.add(pR1);
            d1.add(pR2);
            d1.add(pLFSR1);
            d1.add(pLFSR2);
            d1.add(pLFSR3);
            d1.add(pFile);
            

            generatePrint(pArray,"pArray");
            generatePrint(pString,"pString");
            generatePrint(pR1,"pR1");
            generatePrint(pR2,"pR2");
            generatePrint(pLFSR1,"pLFSR1");
            generatePrint(pLFSR2,"pLFSR2");
            generatePrint(pLFSR3,"pLFSR3");
            generatePrint(pFile,"pFile");
            generatePrint(d1,"d1");
                                       
            byte[] array1 = {1, 2};
            byte[] array2 = {3, 4};
            byte[] array3 = {5, 6};
            byte[] array4 = {7, 8};
            byte[] array5 = {9, 0};
            
            Packet p1 = new Packet(array1);
            Packet p2 = new Packet(array2);
            Packet p3 = new Packet(array3);
            Packet p4 = new Packet(array4);
            Packet p5 = new Packet(array5);

            Distribution dLoop1 = new Distribution(3);
            dLoop1.add(p2);
            dLoop1.add(p3);
            dLoop1.add(p4);

            Distribution dLoop2 = new Distribution(5);
            dLoop2.add(p1);
            dLoop2.add(dLoop1);
            dLoop2.add(p5);

            generatePrint(dLoop2,"dLoop2");


            

            

            
        }
        catch (FileNotFoundException e)
        {
            System.err.println(e);
        }
    
    }

    public static void generatePrint(Streamable s, String name) {
        byte[] output = new byte[totalLength];
        int offset = 0;
        int length = totalLength;

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

