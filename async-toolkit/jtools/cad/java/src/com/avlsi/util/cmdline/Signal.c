#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#ifndef FULCRUM_EXCLUDED_GENERATED_INCLUDES
#include "Signal.h"
#endif

JavaVM *jvm = NULL;
int SignalSocket=0;
int Port=0;
FILE* SignalFD;

void SendSignalSocket(char* msg) ;
void sendSignal(jobject class) ;

/* A Handler */
//static jobject handler_interface=NULL;
//static jmethodID signal_func;
//static jobject sigint_class;
//static jobject sighup_class;
//static jobject sigquit_class;
//static jobject sigusr1_class;
//static jobject sigusr2_class;

typedef void (*sigfunc)();

void sigint() { 
    //signal(SIGINT, sigint);
    SendSignalSocket("SIGINT\n"); 
}
void sighup() { 
    //signal(SIGHUP, sighup);
    SendSignalSocket("SIGHUP\n"); 
}
void sigquit() { 
    //signal(SIGQUIT, sigquit);
    SendSignalSocket("SIGQUIT\n"); 
}
void sigusr1() { 
    //signal(SIGUSR1, sigusr1);
    SendSignalSocket("SIGUSR1\n"); 
}
void sigusr2() { 
    //signal(SIGUSR2, sigusr2);
    SendSignalSocket("SIGUSR2\n");
}

void validate(void* object, char* msg) {
    if (!object) {
        fprintf(stderr, "Fatal error Signal.setHandler: %s not found\n", msg);
        fflush(stderr);
        exit(1);
    }
}


void OpenSignalSocket(int port) {
    int len;
    struct sockaddr_in sa;
    struct hostent *he;
    if (SignalSocket) { return; }
    printf("Opening signal socket on port %d\n", port);
    he=gethostbyname("localhost");
    sa.sin_family = he->h_addrtype;
    sa.sin_port=htons(port);
    bzero(&sa,sizeof(sa));
    bcopy(he->h_addr,(char *)&sa.sin_addr,he->h_length);
    //if ( (SignalSocket=socket(AF_INET, SOCK_STREAM, 0)) <0) {
    if ( (SignalSocket=socket(sa.sin_family, SOCK_STREAM, 0)) <0) {
        printf("ERROR: couldn't get signal socket on port %d\n", port);
        perror("");
        SignalSocket=0;
        return;
    }
    //sa.sin_family = AF_INET;
    //sa.sin_port=port;
    //sa.sin_addr.s_addr=INADDR_LOOPBACK;
    len=sizeof(struct sockaddr_in);
    if (connect(SignalSocket, &sa, len) < 0) {
        printf("ERROR: couldn't open signal socket on port %d\n", port);
        perror("");
        SignalSocket=0;
        return;
    }
    SignalFD = fdopen(SignalSocket, "r");
}
//void CloseSignalSocket() { close(SignalSocket); }
void SendSignalSocket(char* msg) {
    static int infunc=0;
    int c;
    if (infunc) { return; }
    infunc=1;
    OpenSignalSocket(Port);
    printf("Sending message %s\n", msg);
    send(SignalSocket, msg, strlen(msg), 0);
    printf("Returned from sending message %s\n", msg);
    c=fgetc(SignalFD);
    printf("Got return character from signal socket: %d\n", c);
    infunc=0;
}

/*
 * Class:     com_avlsi_util_cmdline_Signal
 * Method:    setHandler
 * Signature: (II)V
 */
JNIEXPORT void JNICALL Java_com_avlsi_util_cmdline_Signal_nativeSetHandler
  (JNIEnv *env, jobject class, jint jsig, jint port) {
    //jobject *ob, oldOb;
    int sig;
    struct sigaction newAct, oldAct;
    sigfunc fun;
    Port=port;
    //OpenSignalSocket(port);
//    if (!jvm) {
//        (*env)->GetJavaVM(env, &jvm);
//        validate(jvm, "JVM");
//    }
//    if (!handler_interface) {
//        handler_interface = (*env)->FindClass(env, "com/avlsi/util/cmdline/Signal$Handler");
//        handler_interface = (jclass) (*env)->NewGlobalRef(env, (jobject) handler_interface);
//        validate(handler_interface, "Interface Signal.Handler");
//        signal_func = (*env)->GetMethodID(env, handler_interface, "execute", "()V");
//        validate(signal_func, "method Handler.execute");
//                
//    }
    switch (jsig) {
    case  com_avlsi_util_cmdline_Signal_SIGINT:
        sig=SIGINT; fun=sigint; break;
        //ob=&sigint_class; sig=SIGINT; fun=sigint; break;
    case  com_avlsi_util_cmdline_Signal_SIGHUP:
        sig=SIGHUP; fun=sighup; break;
        //ob=&sighup_class; sig=SIGHUP; fun=sighup; break;
    case  com_avlsi_util_cmdline_Signal_SIGQUIT:
        sig=SIGQUIT; fun=sigquit; break;
        //ob=&sigquit_class; sig=SIGQUIT; fun=sigquit; break;
    case  com_avlsi_util_cmdline_Signal_SIGUSR1:
        sig=SIGUSR1; fun=sigusr1; break;
        //ob=&sigusr1_class; sig=SIGUSR1; fun=sigusr1; break;
    case  com_avlsi_util_cmdline_Signal_SIGUSR2:
        sig=SIGUSR2; fun=sigusr2; break;
        //ob=&sigusr2_class; sig=SIGUSR2; fun=sigusr2; break;
    default:
        return; // TODO throw some approp exception
    }
//    oldOb = *ob;
//    if (!handler) {
//        *ob=handler;
//        // restore default handler
//        signal(sig, SIG_DFL);
//    } else {
//        signal(sig, fun);
//        *ob = (*env)->NewGlobalRef(env, handler);
//    }
//    if (oldOb) { (*env)->DeleteGlobalRef(env, oldOb); }
    newAct.sa_handler=fun;
    sigemptyset(&newAct.sa_mask);
    sigaddset(&newAct.sa_mask, sig);
    newAct.sa_flags |= SA_RESTART;
    if (port) {
        sigaction(sig, &newAct, &oldAct);
        //signal(sig, fun);
    } else {
        signal(sig, SIG_DFL);
    }
}

//void sendSignal(jobject class) {
//    JNIEnv *env;
//    int status; 
//    
//    validate(jvm, "JVM");
//    validate(handler_interface, "Interface Signal.Handler");
//    validate(signal_func, "method Handler.execute");
//
//    printf("Attaching signal thread.\n");
//    status = (*jvm)->AttachCurrentThread(jvm, (void **) &env, NULL);
//    assert (status == 0);
//    printf("Attach returned: %d.\n", status );
//    printf("Calling into JVM for object signal handler.\n");
//    (*env)->CallVoidMethod(env, class, signal_func);
//    printf("Call returned from JVM.\n");
//    // Detaching causes a crash, since this thread came from the VM
//    //   and Attach/Detach are absolute, not incremental
//    //(*jvm)->DetachCurrentThread(jvm);
//}


  
