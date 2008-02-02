# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-msse2 -axTPSW -S";
	.file "indexeddot_sp.f"
	.text
..TXTST0:
# -- Begin  indexeddot_sp_
# mark_begin;
       .align    2,0x90
	.globl indexeddot_sp_
indexeddot_sp_:
# parameter 1: 24 + %esp
# parameter 2: 28 + %esp
# parameter 3: 32 + %esp
# parameter 4: 36 + %esp
..B1.1:                         # Preds ..B1.0
        subl      $20, %esp                                     #4.21
        movl      24(%esp), %ecx                                #4.21
        movl      32(%esp), %eax                                #4.21
        movl      (%eax), %edx                                  #11.10
        testl     %edx, %edx                                    #11.10
        jle       ..B1.6        # Prob 2%                       #11.10
                                # LOE edx ecx ebx ebp esi edi
..B1.2:                         # Preds ..B1.1
        movl      %ebx, 16(%esp)                                #
        movl      %esi, 12(%esp)                                #
        movl      36(%esp), %esi                                #
        movl      %edi, 8(%esp)                                 #
        pxor      %xmm0, %xmm0                                  #
        movl      $1, %eax                                      #
        movl      28(%esp), %edi                                #
                                # LOE eax edx ecx ebp esi edi xmm0
..B1.3:                         # Preds ..B1.3 ..B1.2
        movl      -4(%edi,%eax,4), %ebx                         #12.22
        movss     -4(%ecx,%ebx,4), %xmm1                        #12.22
        mulss     -4(%esi,%eax,4), %xmm1                        #12.32
        addl      $1, %eax                                      #11.10
        cmpl      %edx, %eax                                    #11.10
        addss     %xmm1, %xmm0                                  #12.10
        jle       ..B1.3        # Prob 82%                      #11.10
                                # LOE eax edx ecx ebp esi edi xmm0
..B1.4:                         # Preds ..B1.3
        movl      16(%esp), %ebx                                #
        movl      12(%esp), %esi                                #
        movl      8(%esp), %edi                                 #
                                # LOE ebx ebp esi edi xmm0
..B1.5:                         # Preds ..B1.4 ..B1.6
        movss     %xmm0, (%esp)                                 #15.7
        flds      (%esp)                                        #15.7
        addl      $20, %esp                                     #15.7
        ret                                                     #15.7
                                # LOE
..B1.6:                         # Preds ..B1.1                  # Infreq
        pxor      %xmm0, %xmm0                                  #
        jmp       ..B1.5        # Prob 100%                     #
        .align    2,0x90
                                # LOE ebx ebp esi edi xmm0
# mark_end;
	.type	indexeddot_sp_,@function
	.size	indexeddot_sp_,.-indexeddot_sp_
	.data
# -- End  indexeddot_sp_
	.data
	.section .note.GNU-stack, ""
# End
