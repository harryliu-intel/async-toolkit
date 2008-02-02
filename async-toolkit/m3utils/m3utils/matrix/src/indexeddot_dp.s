# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "indexeddot_dp.f"
	.text
..TXTST0:
# -- Begin  indexeddot_dp_
# mark_begin;
       .align    2,0x90
	.globl indexeddot_dp_
indexeddot_dp_:
# parameter 1: 8 + %ebp
# parameter 2: 12 + %ebp
# parameter 3: 16 + %ebp
# parameter 4: 20 + %ebp
..B1.1:                         # Preds ..B1.0
        pushl     %ebp                                          #4.33
        movl      %esp, %ebp                                    #4.33
        andl      $-8, %esp                                     #4.33
        subl      $24, %esp                                     #4.33
        movl      8(%ebp), %ecx                                 #4.33
        movl      16(%ebp), %eax                                #4.33
        movl      (%eax), %edx                                  #11.10
        testl     %edx, %edx                                    #11.10
        jle       ..B1.6        # Prob 2%                       #11.10
                                # LOE edx ecx ebx esi edi
..B1.2:                         # Preds ..B1.1
        movl      %esi, 16(%esp)                                #
        movl      20(%ebp), %esi                                #
        movl      %edi, 12(%esp)                                #
        movl      12(%ebp), %edi                                #
        movl      %ebx, 8(%esp)                                 #
        movl      $1, %eax                                      #
        pxor      %xmm0, %xmm0                                  #
                                # LOE eax edx ecx esi edi xmm0
..B1.3:                         # Preds ..B1.3 ..B1.2
        movl      -4(%edi,%eax,4), %ebx                         #12.22
        movsd     -8(%ecx,%ebx,8), %xmm1                        #12.22
        mulsd     -8(%esi,%eax,8), %xmm1                        #12.32
        addl      $1, %eax                                      #11.10
        cmpl      %edx, %eax                                    #11.10
        addsd     %xmm1, %xmm0                                  #12.10
        jle       ..B1.3        # Prob 82%                      #11.10
                                # LOE eax edx ecx esi edi xmm0
..B1.4:                         # Preds ..B1.3
        movl      16(%esp), %esi                                #
        movl      12(%esp), %edi                                #
        movl      8(%esp), %ebx                                 #
                                # LOE ebx esi edi xmm0
..B1.5:                         # Preds ..B1.4 ..B1.6
        movsd     %xmm0, (%esp)                                 #15.7
        fldl      (%esp)                                        #15.7
        movl      %ebp, %esp                                    #15.7
        popl      %ebp                                          #15.7
        ret                                                     #15.7
                                # LOE
..B1.6:                         # Preds ..B1.1                  # Infreq
        pxor      %xmm0, %xmm0                                  #
        jmp       ..B1.5        # Prob 100%                     #
        .align    2,0x90
                                # LOE ebx esi edi xmm0
# mark_end;
	.type	indexeddot_dp_,@function
	.size	indexeddot_dp_,.-indexeddot_dp_
	.data
# -- End  indexeddot_dp_
	.data
	.section .note.GNU-stack, ""
# End
