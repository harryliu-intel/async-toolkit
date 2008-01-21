# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "muld.f"
	.text
..TXTST0:
# -- Begin  muld_
# mark_begin;
       .align    2,0x90
	.globl muld_
muld_:
# parameter 1: 24 + %esp
# parameter 2: 28 + %esp
# parameter 3: 32 + %esp
# parameter 4: 36 + %esp
# parameter 5: 40 + %esp
# parameter 6: 44 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %edi                                          #4.18
        pushl     %ebp                                          #4.18
        subl      $12, %esp                                     #4.18
        movl      36(%esp), %eax                                #4.18
        cvtsi2ssl (%eax), %xmm0                                 #11.10
        movl      44(%esp), %edx                                #4.18
        movss     _2il0floatpacket.1, %xmm2                     #11.10
        movl      (%edx), %edx                                  #4.18
        subss     %xmm2, %xmm0                                  #11.10
        addss     %xmm2, %xmm0                                  #11.10
        cvttss2si %xmm0, %edi                                   #11.10
        testl     %edi, %edi                                    #11.10
        jle       ..B1.9        # Prob 1%                       #11.10
                                # LOE edx ebx esi edi xmm2
..B1.2:                         # Preds ..B1.1
        movl      32(%esp), %eax                                #
        cvtsi2ssl %edx, %xmm0                                   #12.13
        movaps    %xmm2, %xmm1                                  #
        xorl      %ebp, %ebp                                    #
        movl      %ebx, (%esp)                                  #17.13
        subss     %xmm2, %xmm0                                  #12.13
        movl      %esi, 4(%esp)                                 #17.13
        addss     %xmm2, %xmm0                                  #12.13
        cvttss2si %xmm0, %ecx                                   #12.13
        shll      $3, %edx                                      #17.13
        subl      %edx, %eax                                    #
        pxor      %xmm0, %xmm0                                  #17.13
                                # LOE eax edx ecx ebp edi xmm0 xmm1 xmm2
..B1.3:                         # Preds ..B1.7 ..B1.2
        testl     %ecx, %ecx                                    #12.13
        jle       ..B1.7        # Prob 1%                       #12.13
                                # LOE eax edx ecx ebp edi xmm0 xmm1 xmm2
..B1.4:                         # Preds ..B1.3
        cvttss2si %xmm1, %ebx                                   #17.13
        imull     %edx, %ebx                                    #17.13
        movaps    %xmm2, %xmm3                                  #
        movl      %edi, 8(%esp)                                 #
        xorl      %esi, %esi                                    #
        addl      %eax, %ebx                                    #
                                # LOE eax edx ecx ebx ebp esi xmm0 xmm1 xmm2 xmm3
..B1.5:                         # Preds ..B1.5 ..B1.4
        cvttss2si %xmm3, %edi                                   #17.13
        movsd     %xmm0, -8(%ebx,%edi,8)                        #17.13
        addss     %xmm2, %xmm3                                  #12.13
        addl      $1, %esi                                      #12.13
        cmpl      %ecx, %esi                                    #12.13
        jb        ..B1.5        # Prob 99%                      #12.13
                                # LOE eax edx ecx ebx ebp esi xmm0 xmm1 xmm2 xmm3
..B1.6:                         # Preds ..B1.5
        movl      8(%esp), %edi                                 #
                                # LOE eax edx ecx ebp edi xmm0 xmm1 xmm2
..B1.7:                         # Preds ..B1.6 ..B1.3
        addss     %xmm2, %xmm1                                  #11.10
        addl      $1, %ebp                                      #11.10
        cmpl      %edi, %ebp                                    #11.10
        jb        ..B1.3        # Prob 99%                      #11.10
                                # LOE eax edx ecx ebp edi xmm0 xmm1 xmm2
..B1.8:                         # Preds ..B1.7
        movl      (%esp), %ebx                                  #
        movl      4(%esp), %esi                                 #
                                # LOE ebx esi
..B1.9:                         # Preds ..B1.8 ..B1.1
        addl      $12, %esp                                     #20.7
        popl      %ebp                                          #20.7
        popl      %edi                                          #20.7
        ret                                                     #20.7
        .align    2,0x90
                                # LOE
# mark_end;
	.type	muld_,@function
	.size	muld_,.-muld_
	.data
# -- End  muld_
	.section .rodata, "a"
	.align 4
	.align 4
_2il0floatpacket.1:
	.long	0x3f800000
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,4
	.data
	.section .note.GNU-stack, ""
# End
