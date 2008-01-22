# -- Machine type IA32
# mark_description "Intel(R) Fortran Compiler for applications running on IA-32, Version 10.1    Build 20070913 %s";
# mark_description "-S";
	.file "mulmv_sp.f"
	.text
..TXTST0:
# -- Begin  mulmv_sp_
# mark_begin;
       .align    2,0x90
	.globl mulmv_sp_
mulmv_sp_:
# parameter 1: 20 + %esp
# parameter 2: 24 + %esp
# parameter 3: 28 + %esp
# parameter 4: 32 + %esp
# parameter 5: 36 + %esp
..B1.1:                         # Preds ..B1.0
        pushl     %edi                                          #4.18
        pushl     %esi                                          #4.18
        pushl     %ebp                                          #4.18
        pushl     %ebx                                          #4.18
        movl      24(%esp), %edi                                #4.18
        movl      32(%esp), %eax                                #4.18
        movl      36(%esp), %ecx                                #4.18
        movl      (%ecx), %esi                                  #4.18
        movl      (%eax), %edx                                  #10.10
        movl      %esi, %ecx                                    #13.27
        shll      $2, %ecx                                      #13.27
        testl     %edx, %edx                                    #10.10
        jle       ..B1.8        # Prob 2%                       #10.10
                                # LOE edx ecx ebx ebp esi edi
..B1.2:                         # Preds ..B1.1
        fldz                                                    #11.10
        movl      $1, %eax                                      #
                                # LOE eax edx ecx esi edi f2
..B1.3:                         # Preds ..B1.9 ..B1.6 ..B1.2
        fld       %st(0)                                        #11.10
        testl     %esi, %esi                                    #12.13
        jle       ..B1.9        # Prob 2%                       #12.13
                                # LOE eax edx ecx esi f1 f2
..B1.4:                         # Preds ..B1.3
        movl      20(%esp), %ebx                                #
        movl      $1, %ebp                                      #
        subl      %ecx, %ebx                                    #
        movl      %ecx, %edi                                    #
        imull     %eax, %edi                                    #
        addl      %edi, %ebx                                    #
        movl      24(%esp), %edi                                #
                                # LOE eax edx ecx ebx ebp esi edi f1 f2
..B1.5:                         # Preds ..B1.5 ..B1.4
        jmp       ..L1          # Prob 0%                       #
..L2:                                                           #
        faddp     %st, %st(1)                                   #
..L1:                                                           #
        flds      -4(%ebx,%ebp,4)                               #13.27
        fmuls     -4(%edi,%ebp,4)                               #13.33
        addl      $1, %ebp                                      #12.13
        cmpl      %esi, %ebp                                    #12.13
        jle       ..L2          # Prob 82%                      #12.13
        faddp     %st, %st(1)                                   #13.13
                                # LOE eax edx ecx ebx ebp esi edi f1 f2
..B1.6:                         # Preds ..B1.5
        movl      28(%esp), %ebx                                #13.13
        fstps     -4(%ebx,%eax,4)                               #13.13
        addl      $1, %eax                                      #10.10
        cmpl      %edx, %eax                                    #10.10
        jle       ..B1.3        # Prob 82%                      #10.10
                                # LOE eax edx ecx esi f2
..B1.7:                         # Preds ..B1.6                  # Infreq
        fstp      %st(0)                                        #
                                # LOE ebx ebp
..B1.8:                         # Preds ..B1.7 ..B1.1           # Infreq
        popl      %ebx                                          #16.7
        popl      %ebp                                          #16.7
        popl      %esi                                          #16.7
        popl      %edi                                          #16.7
        ret                                                     #16.7
                                # LOE
..B1.9:                         # Preds ..B1.3                  # Infreq
        fstp      %st(0)                                        #
        movl      28(%esp), %ebx                                #13.13
        fsts      -4(%ebx,%eax,4)                               #13.13
        addl      $1, %eax                                      #10.10
        cmpl      %edx, %eax                                    #10.10
        jle       ..B1.3        # Prob 82%                      #10.10
                                # LOE eax edx ecx esi f2
..B1.10:                        # Preds ..B1.9                  # Infreq
        fstp      %st(0)                                        #
        popl      %ebx                                          #
        popl      %ebp                                          #
        popl      %esi                                          #
        popl      %edi                                          #
        ret                                                     #
        .align    2,0x90
                                # LOE ebx ebp
# mark_end;
	.type	mulmv_sp_,@function
	.size	mulmv_sp_,.-mulmv_sp_
	.data
# -- End  mulmv_sp_
	.data
	.section .note.GNU-stack, ""
# End
