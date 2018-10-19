import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ListIterator;

import org.bridj.IntValuedEnum;
import org.bridj.Pointer;

import HlpWm.WmLibrary;
import HlpWm.fm_libCfg;
import HlpWm.fm_modelMessage;
import HlpWm.fm_socket;

public class BridJWm
{
    public static short ntohs(short in)
    {
        ByteBuffer buf = ByteBuffer.allocate(2); // short = 2 bytes
        buf.order(ByteOrder.BIG_ENDIAN); // network side
        buf.clear();
        buf.putShort(0, in);
        buf.order(ByteOrder.nativeOrder()); // host side (LITTLE_ENDIAN on x86)
        short out = buf.getShort(0);
        return out;
    }

    public static int ntohl(int in)
    {
        ByteBuffer buf = ByteBuffer.allocate(4); // long = 4 bytes
        buf.order(ByteOrder.BIG_ENDIAN); // network side
        buf.clear();
        buf.putInt(0, in);
        buf.order(ByteOrder.nativeOrder()); // host side (LITTLE_ENDIAN on x86)
        int out = buf.getInt(0);
        return out;
    }

    public static void main(String[] args)
    {
        String testName = "dat/VlanTagging_hlp-model_519606380";

        byte msgEchoMode   = WmLibrary.FALSE;
        byte msgDumpMode   = WmLibrary.FALSE;
        byte msgInjectMode = WmLibrary.TRUE;
        byte sendIntr      = WmLibrary.TRUE;

        int status = WmLibrary.FM_OK;

        WmLibrary.setMsgEchoMode   (msgEchoMode);
        WmLibrary.setMsgDumpMode   (msgDumpMode);
        WmLibrary.setMsgInjectMode (msgInjectMode);

        System.out.printf("\n\n");

        String iMsgFile = testName + ".imsg";
        Pointer<Byte> iMsgFilePtr = Pointer.pointerToCString(iMsgFile);
        WmLibrary.msg_direction_t ingress = WmLibrary.msg_direction_t.INGRESS;
        status = WmLibrary.openMsgInject(ingress, iMsgFilePtr);
        if (status == WmLibrary.FM_OK) 
            System.out.println("Opened file \'" + iMsgFile +  "\' for reading.");
        else
            System.out.println("ERROR: could not open file \'" + iMsgFile + "\' for reading!");

        String eMsgFile = testName + ".emsg";
        Pointer<Byte> eMsgFilePtr = Pointer.pointerToCString(eMsgFile);
        WmLibrary.msg_direction_t egress = WmLibrary.msg_direction_t.EGRESS;
        status = WmLibrary.openMsgInject(egress, eMsgFilePtr);
        if (status == WmLibrary.FM_OK) 
            System.out.println("Opened file \'" + eMsgFile + "\' for reading.");
        else
            System.out.println("ERROR: could not open file \'" + eMsgFile + "\' for reading!");

        int sw = 0;
        fm_libCfg libCfg = new fm_libCfg();
        Pointer<fm_libCfg> libCfgPtr = Pointer.getPointer(libCfg);
        status = WmLibrary.fmModelLibInit(sw, libCfgPtr);
        if (status != WmLibrary.FM_OK) 
            System.out.println("FAILED to init model lib: err = " + status);

        status = WmLibrary.fmModelReset(sw);
        if (status != WmLibrary.FM_OK) 
            System.out.println("FAILED to reset chip: err = " + status);

        Integer m = WmLibrary.UNDEF_VAL;
        Pointer<Integer> alBitmaskPtr = Pointer.pointerToInts(m, m, m, m, m, m, m, m);
        Pointer<Byte> nvmImgFilePtr = Pointer.pointerToCString(WmLibrary.FM_NVM_IMG_FILE_NAME);
        status = WmLibrary.loadImg(nvmImgFilePtr, alBitmaskPtr);
        if (status != WmLibrary.FM_OK) 
            System.out.println("FAILED to load NVM image: err = " + status);

/*        Pointer<Byte> parserCfgFilePtr = Pointer.pointerToCString("");
        status = WmLibrary.loadParserCfg(parserCfgFilePtr);
        if (status != WmLibrary.FM_OK) 
            System.out.println("FAILED to load parser config: err = " + status);
*/
        int msgNumClients = 1;
        int numSockets = 2 + msgNumClients;

        WmLibrary.initPhysicalPorts();

        int arrayLength = WmLibrary.MAX_PERSISTENT_CONNECTIONS + 1;
        Pointer< Pointer<fm_socket> > socketsPtrPtr = Pointer.allocatePointers(fm_socket.class, arrayLength);
        for (ListIterator< Pointer<fm_socket> > it = socketsPtrPtr.iterator(); it.hasNext();)
        {
            it.next();
            it.set(null);
            // fm_socket s = new fm_socket();
            // it.set(Pointer.getPointer(s));
        }

        /* DBG_AKA */
        Integer msg_cnt = 0;
        /* END DBG_AKA */
        
        while (true)
        {
            if (sendIntr == WmLibrary.TRUE)
                WmLibrary.handleIntr(numSockets, socketsPtrPtr);

            fm_modelMessage iMsg = new fm_modelMessage();
            Pointer<fm_modelMessage > iMsgPtr = Pointer.getPointer(iMsg);
            status = WmLibrary.readMsgInject(ingress, iMsgPtr); // inject ingress message from file    
            if (status != WmLibrary.FM_OK) 
                break;

            short socketType = ntohs(iMsg.type());      
            short serverPort = ntohs(iMsg.port());
            int    msgLength = ntohl(iMsg.msgLength());
            IntValuedEnum<WmLibrary.fm_socketType> socketTypeEnum
                = WmLibrary.fm_socketType.fromValue(socketType);

            fm_socket socket = new fm_socket(); // fake socket
            socket.sock       (-1);
            socket.type       (socketTypeEnum);
            socket.serverPort (serverPort);

            /* DBG_AKA */
            //System.out.println("Msg " + msg_cnt + ": Type " + socketTypeEnum.value());
            //msg_cnt++;
            /* END DBG_AKA */

            Pointer<fm_socket> socketPtr = Pointer.getPointer(socket);
            status = WmLibrary.ProcessMessage(socketPtr, iMsgPtr, msgLength);
            if (status != WmLibrary.FM_OK)
                break;
        }

        status = WmLibrary.closeMsgInject(ingress);
        status = WmLibrary.closeMsgInject(egress);

        // Get egress message match counts:
        int eMsgMismatchCount = WmLibrary.getMsgMismatchCount();
        int eMsgMatchCount = WmLibrary.getMsgMatchCount();
        int eMsgTotalCount = eMsgMismatchCount + eMsgMatchCount;

        status = (eMsgMismatchCount == 0) ? WmLibrary.FM_OK : WmLibrary.FM_FAIL;
        String checkStatus = (status == WmLibrary.FM_OK) ? "passed." : "FAILED!";

        System.out.printf("Check egress messages:  %10d mismatch / %10d match / %10d total :  test %s\n",
                          eMsgMismatchCount, eMsgMatchCount, eMsgTotalCount, checkStatus);
    }
};
