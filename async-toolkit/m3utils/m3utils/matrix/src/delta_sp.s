# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "delta_sp.f"
	.text
..TXTST0:
# -- Begin  delta_sp_
# mark_begin;
       .align    2,0x90
	.globl delta_sp_
delta_sp_:
# parameter 1: 12 + %esp
# parameter 2: 16 + %esp
# parameter 3: 20 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %edi                                          #4.18
        pushl     %ebx                                          #4.18
        movl      12(%esp), %ebx                                #4.18
        movl      20(%esp), %eax                                #4.18
        movl      (%eax), %edx                                  #9.10
        testl     %edx, %edx                                    #9.10
        jle       ..B1.15       # Prob 50%                      #9.10
                                # LOE edx ebx ebp esi
..B1.2:                         # Preds ..B1.1
        movl      %ebx, %ecx                                    #9.10
        andl      $63, %ecx                                     #9.10
        je        ..B1.5        # Prob 50%                      #9.10
                                # LOE edx ecx ebx ebp esi
..B1.3:                         # Preds ..B1.2
        testb     $3, %cl                                       #9.10
        jne       ..B1.16       # Prob 10%                      #9.10
                                # LOE edx ecx ebx ebp esi
..B1.4:                         # Preds ..B1.3
        negl      %ecx                                          #9.10
        addl      $64, %ecx                                     #9.10
        shrl      $2, %ecx                                      #9.10
                                # LOE edx ecx ebx ebp esi
..B1.5:                         # Preds ..B1.4 ..B1.2
        lea       16(%ecx), %eax                                #9.10
        cmpl      %eax, %edx                                    #9.10
        jl        ..B1.16       # Prob 10%                      #9.10
                                # LOE edx ecx ebx ebp esi
..B1.6:                         # Preds ..B1.5
        movl      %edx, %eax                                    #9.10
        subl      %ecx, %eax                                    #9.10
        andl      $15, %eax                                     #9.10
        negl      %eax                                          #9.10
        addl      %edx, %eax                                    #9.10
        testl     %ecx, %ecx                                    #9.10
        ja        ..B1.7        # Prob 99%                      #9.10
                                # LOE eax edx ecx ebx ebp esi
..B1.7:                         # Preds ..B1.6 ..B1.6
        movl      16(%esp), %edi                                #
        lea       (%edi,%edx,4), %edi                           #
                                # LOE eax edx ecx ebx ebp esi edi
..B1.9:                         # Preds ..B1.7 ..B1.9
        movsd     52(%ebx,%ecx,4), %xmm0                        #10.17
        movss     60(%ebx,%ecx,4), %xmm1                        #10.17
        movhps    64(%ebx,%ecx,4), %xmm1                        #10.17
        shufps    $132, %xmm1, %xmm0                            #10.17
        subps     48(%ebx,%ecx,4), %xmm0                        #10.10
        addl      $16, %ecx                                     #9.10
        cmpl      %eax, %ecx                                    #9.10
        jb        ..B1.9        # Prob 99%                      #9.10
                                # LOE eax edx ecx ebx ebp esi edi xmm0
..B1.10:                        # Preds ..B1.9
        shufps    $3, %xmm0, %xmm0                              #10.10
        movss     %xmm0, -4(%edi)                               #10.10
                                # LOE eax edx ebx ebp esi
..B1.11:                        # Preds ..B1.10 ..B1.16
        cmpl      %edx, %eax                                    #9.10
        jae       ..B1.15       # Prob 1%                       #9.10
                                # LOE eax edx ebx ebp esi
..B1.13:                        # Preds ..B1.11 ..B1.13
        movss     4(%ebx,%eax,4), %xmm0                         #10.17
        subss     (%ebx,%eax,4), %xmm0                          #10.10
        addl      $1, %eax                                      #9.10
        cmpl      %edx, %eax                                    #9.10
        jb        ..B1.13       # Prob 99%                      #9.10
                                # LOE eax edx ebx ebp esi xmm0
..B1.14:                        # Preds ..B1.13
        movl      16(%esp), %eax                                #10.10
        movss     %xmm0, -4(%eax,%edx,4)                        #10.10
                                # LOE ebp esi
..B1.15:                        # Preds ..B1.14 ..B1.11 ..B1.1
        popl      %ebx                                          #12.7
        popl      %edi                                          #12.7
        ret                                                     #12.7
                                # LOE
..B1.16:                        # Preds ..B1.5 ..B1.3           # Infreq
        xorl      %eax, %eax                                    #9.10
        jmp       ..B1.11       # Prob 100%                     #9.10
        .align    2,0x90
                                # LOE eax edx ebx ebp esi
# mark_end;
	.type	delta_sp_,@function
	.size	delta_sp_,.-delta_sp_
	.data
# -- End  delta_sp_
	.data
	.section .note.GNU-stack, ""
# End
