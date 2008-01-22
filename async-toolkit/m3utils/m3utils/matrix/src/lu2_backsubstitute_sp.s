	.file	"lu2_backsubstitute_sp.f"
	.version	"01.01"
.stabs "/home/mika/t/matrix/src/",100,0,0,.Ltext0
.stabs "lu2_backsubstitute_sp.f",100,0,0,.Ltext0
.text
.Ltext0:
	.stabs	"gcc2_compiled.", 0x3c, 0, 0, 0
.stabs "int:t(0,1)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "char:t(0,2)=r(0,2);0;255;",128,0,0,0
.stabs "long int:t(0,3)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "unsigned int:t(0,4)=r(0,1);0000000000000;0037777777777;",128,0,0,0
.stabs "long unsigned int:t(0,5)=r(0,1);0000000000000;0037777777777;",128,0,0,0
.stabs "long long int:t(0,6)=r(0,1);01000000000000000000000;0777777777777777777777;",128,0,0,0
.stabs "long long unsigned int:t(0,7)=r(0,1);0000000000000;01777777777777777777777;",128,0,0,0
.stabs "short int:t(0,8)=r(0,8);-32768;32767;",128,0,0,0
.stabs "short unsigned int:t(0,9)=r(0,9);0;65535;",128,0,0,0
.stabs "signed char:t(0,10)=r(0,10);-128;127;",128,0,0,0
.stabs "unsigned char:t(0,11)=r(0,11);0;255;",128,0,0,0
.stabs "float:t(0,12)=r(0,1);4;0;",128,0,0,0
.stabs "double:t(0,13)=r(0,1);8;0;",128,0,0,0
.stabs "long double:t(0,14)=r(0,1);12;0;",128,0,0,0
.stabs "complex int:t(0,15)=s8real:(0,1),0,32;imag:(0,1),32,32;;",128,0,0,0
.stabs "complex float:t(0,16)=r(0,16);4;0;",128,0,0,0
.stabs "complex double:t(0,17)=r(0,17);8;0;",128,0,0,0
.stabs "complex long double:t(0,18)=r(0,18);12;0;",128,0,0,0
.stabs "void:t(0,19)=(0,19)",128,0,0,0
.stabs "integer:t(0,20)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "unsigned:t(0,21)=r(0,1);0000000000000;0037777777777;",128,0,0,0
.stabs "byte:t(0,22)=r(0,22);-128;127;",128,0,0,0
.stabs "unsigned byte:t(0,23)=r(0,23);0;255;",128,0,0,0
.stabs "word:t(0,24)=r(0,24);-32768;32767;",128,0,0,0
.stabs "unsigned word:t(0,25)=r(0,25);0;65535;",128,0,0,0
.stabs "integer4:t(0,26)=r(0,1);01000000000000000000000;0777777777777777777777;",128,0,0,0
.stabs "unsigned4:t(0,27)=r(0,1);0000000000000;01777777777777777777777;",128,0,0,0
.stabs "logical:t(0,28)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "logical2:t(0,29)=r(0,29);-128;127;",128,0,0,0
.stabs "logical3:t(0,30)=r(0,30);-32768;32767;",128,0,0,0
.stabs "logical4:t(0,31)=r(0,1);01000000000000000000000;0777777777777777777777;",128,0,0,0
.stabs "real:t(0,32)=r(0,1);4;0;",128,0,0,0
.stabs "double precision:t(0,33)=r(0,1);8;0;",128,0,0,0
.stabs "complex:t(0,34)=r(0,34);4;0;",128,0,0,0
.stabs "double complex:t(0,35)=r(0,35);8;0;",128,0,0,0
.stabs "__g77_f2c_integer:t(0,36)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "__g77_f2c_address:t(0,37)=*(0,10)",128,0,0,0
.stabs "__g77_f2c_real:t(0,38)=r(0,1);4;0;",128,0,0,0
.stabs "__g77_f2c_doublereal:t(0,39)=r(0,1);8;0;",128,0,0,0
.stabs "__g77_f2c_complex:t(0,40)=r(0,40);4;0;",128,0,0,0
.stabs "__g77_f2c_doublecomplex:t(0,41)=r(0,41);8;0;",128,0,0,0
.stabs "__g77_f2c_longint:t(0,42)=r(0,1);01000000000000000000000;0777777777777777777777;",128,0,0,0
.stabs "__g77_f2c_logical:t(0,43)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "__g77_f2c_flag:t(0,44)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "__g77_f2c_ftnlen:t(0,45)=r(0,1);0020000000000;0017777777777;",128,0,0,0
.stabs "__g77_f2c_ftnint:t(0,46)=r(0,1);0020000000000;0017777777777;",128,0,0,0
	.p2align 2,0x90
.stabs "lu2_backsubstitute_sp__:F(0,19)",36,0,4,lu2_backsubstitute_sp__
.stabs "m:p(0,47)=*(0,48)=ar(0,20);1;-1;(0,49)=ar(0,20);1;-1;(0,32)",160,0,4,8
.stabs "indx:p(0,50)=*(0,51)=ar(0,20);1;-1;(0,20)",160,0,4,12
.stabs "b:p(0,52)=*(0,53)=ar(0,20);1;-1;(0,32)",160,0,4,16
.stabs "n:p(0,54)=*(0,20)",160,0,4,20
.globl lu2_backsubstitute_sp__
		.type		 lu2_backsubstitute_sp__,@function
lu2_backsubstitute_sp__:
	pushl %ebp
	movl %esp,%ebp
	subl $348,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
.stabn 68,0,4,.LM1-lu2_backsubstitute_sp__
.LM1:
	movl 20(%ebp),%eax
	movl %eax,-36(%ebp)
	movl -36(%ebp),%edx
	movl (%edx),%edx
	movl %edx,-324(%ebp)
	movl -324(%ebp),%esi
	movl %esi,-40(%ebp)
	movl -40(%ebp),%eax
	sall $5,%eax
	movl %eax,-44(%ebp)
	movl -44(%ebp),%edx
	movl %edx,-328(%ebp)
	movl -324(%ebp),%esi
	leal 0(,%esi,4),%ecx
	movl 20(%ebp),%eax
	movl %eax,-48(%ebp)
	movl -48(%ebp),%edx
	movl (%edx),%ebx
	movl -328(%ebp),%esi
	imull %ebx,%esi
	movl %esi,-332(%ebp)
	movl %ecx,%eax
	imull %ebx,%eax
	movl %eax,-336(%ebp)
	movl 20(%ebp),%edx
	movl %edx,-56(%ebp)
	movl -56(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-52(%ebp)
	movl -52(%ebp),%eax
	movl %eax,-64(%ebp)
	movl -64(%ebp),%edx
	sall $5,%edx
	movl %edx,-68(%ebp)
	movl -68(%ebp),%esi
	movl %esi,-60(%ebp)
	movl -52(%ebp),%eax
	leal 0(,%eax,4),%eax
	movl %eax,-72(%ebp)
	movl 20(%ebp),%edx
	movl %edx,-80(%ebp)
	movl -80(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-76(%ebp)
	movl -76(%ebp),%eax
	movl %eax,-88(%ebp)
	movl -88(%ebp),%edx
	sall $5,%edx
	movl %edx,-92(%ebp)
	movl -92(%ebp),%esi
	movl %esi,-84(%ebp)
	movl -76(%ebp),%eax
	leal 0(,%eax,4),%eax
	movl %eax,-96(%ebp)
.stabn 68,0,4,.LM2-lu2_backsubstitute_sp__
.LM2:
.LBB2:
.stabn 68,0,16,.LM3-lu2_backsubstitute_sp__
.LM3:
	movl $-1,-4(%ebp)
.stabn 68,0,18,.LM4-lu2_backsubstitute_sp__
.LM4:
.LBB3:
	movl 20(%ebp),%edx
	movl %edx,-100(%ebp)
	movl -100(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-104(%ebp)
	movl -104(%ebp),%eax
	movl %eax,-24(%ebp)
	movl $1,-16(%ebp)
	.p2align 2,0x90
.L3:
	decl -24(%ebp)
	cmpl $0,-24(%ebp)
	jge .L6
	jmp .L4
	.p2align 2,0x90
.L6:
.stabn 68,0,25,.LM5-lu2_backsubstitute_sp__
.LM5:
	movl 12(%ebp),%edx
	movl %edx,-108(%ebp)
	movl -16(%ebp),%esi
	decl %esi
	movl %esi,-112(%ebp)
	movl -112(%ebp),%eax
	leal 0(,%eax,4),%eax
	movl %eax,-116(%ebp)
	movl -108(%ebp),%edx
	movl -116(%ebp),%esi
	movl (%esi,%edx),%eax
	incl %eax
	movl %eax,-8(%ebp)
.stabn 68,0,26,.LM6-lu2_backsubstitute_sp__
.LM6:
	movl 16(%ebp),%edx
	movl %edx,-120(%ebp)
	movl -8(%ebp),%esi
	decl %esi
	movl %esi,-124(%ebp)
	movl -124(%ebp),%eax
	leal 0(,%eax,4),%eax
	movl %eax,-128(%ebp)
	movl -120(%ebp),%edx
	movl -128(%ebp),%esi
	flds (%esi,%edx)
	fstps -12(%ebp)
.stabn 68,0,27,.LM7-lu2_backsubstitute_sp__
.LM7:
	movl 16(%ebp),%eax
	movl %eax,-132(%ebp)
	movl -8(%ebp),%edx
	decl %edx
	movl %edx,-136(%ebp)
	movl -136(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-140(%ebp)
	movl 16(%ebp),%eax
	movl %eax,-144(%ebp)
	movl -16(%ebp),%edx
	decl %edx
	movl %edx,-148(%ebp)
	movl -148(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-152(%ebp)
	movl -144(%ebp),%eax
	movl -152(%ebp),%edx
	flds (%edx,%eax)
	movl -132(%ebp),%esi
	movl -140(%ebp),%eax
	fstps (%eax,%esi)
.stabn 68,0,28,.LM8-lu2_backsubstitute_sp__
.LM8:
	cmpl $-1,-4(%ebp)
	je .L7
.stabn 68,0,29,.LM9-lu2_backsubstitute_sp__
.LM9:
.LBB4:
	movl -4(%ebp),%edx
	movl %edx,-156(%ebp)
	movl -156(%ebp),%esi
	incl %esi
	movl %esi,-160(%ebp)
	movl -16(%ebp),%eax
	subl -160(%ebp),%eax
	movl %eax,-164(%ebp)
	movl -164(%ebp),%edx
	incl %edx
	movl %edx,-32(%ebp)
	movl -156(%ebp),%esi
	movl %esi,-20(%ebp)
	.p2align 2,0x90
.L8:
	decl -32(%ebp)
	cmpl $0,-32(%ebp)
	jge .L11
	jmp .L9
	.p2align 2,0x90
.L11:
.stabn 68,0,30,.LM10-lu2_backsubstitute_sp__
.LM10:
	movl 8(%ebp),%eax
	movl %eax,-168(%ebp)
	movl -20(%ebp),%edx
	decl %edx
	movl %edx,-172(%ebp)
	movl -172(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-176(%ebp)
	movl -16(%ebp),%eax
	decl %eax
	movl %eax,-180(%ebp)
	movl -180(%ebp),%edx
	imull %ecx,%edx
	movl %edx,-184(%ebp)
	movl -176(%ebp),%esi
	addl -184(%ebp),%esi
	movl %esi,-188(%ebp)
	movl 16(%ebp),%eax
	movl %eax,-192(%ebp)
	movl -20(%ebp),%edx
	decl %edx
	movl %edx,-196(%ebp)
	movl -196(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-200(%ebp)
	movl -168(%ebp),%eax
	movl -188(%ebp),%edx
	flds (%edx,%eax)
	movl -192(%ebp),%esi
	movl -200(%ebp),%eax
	fmuls (%eax,%esi)
	flds -12(%ebp)
	fsubp %st,%st(1)
	fstps -12(%ebp)
.L12:
.stabn 68,0,31,.LM11-lu2_backsubstitute_sp__
.LM11:
.L10:
	incl -20(%ebp)
	jmp .L8
	.p2align 2,0x90
.L9:
.LBE4:
.stabn 68,0,32,.LM12-lu2_backsubstitute_sp__
.LM12:
	jmp .L13
	.p2align 2,0x90
.L7:
	flds -12(%ebp)
	fldz
	fucompp
	fnstsw %ax
	andb $69,%ah
	cmpb $64,%ah
	je .L13
.stabn 68,0,33,.LM13-lu2_backsubstitute_sp__
.LM13:
	movl -16(%ebp),%edx
	movl %edx,-204(%ebp)
	movl -204(%ebp),%esi
	movl %esi,-4(%ebp)
.L14:
.L13:
.stabn 68,0,35,.LM14-lu2_backsubstitute_sp__
.LM14:
	movl 16(%ebp),%eax
	movl %eax,-208(%ebp)
	movl -16(%ebp),%edx
	decl %edx
	movl %edx,-212(%ebp)
	movl -212(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-216(%ebp)
	flds -12(%ebp)
	movl -208(%ebp),%eax
	movl -216(%ebp),%edx
	fstps (%edx,%eax)
.L15:
.stabn 68,0,36,.LM15-lu2_backsubstitute_sp__
.LM15:
.L5:
	incl -16(%ebp)
	jmp .L3
	.p2align 2,0x90
.L4:
.LBE3:
.stabn 68,0,38,.LM16-lu2_backsubstitute_sp__
.LM16:
.LBB5:
	movl 20(%ebp),%esi
	movl %esi,-224(%ebp)
	movl -224(%ebp),%eax
	movl (%eax),%eax
	movl %eax,-220(%ebp)
	movl $0,-232(%ebp)
	movl -232(%ebp),%edx
	subl -220(%ebp),%edx
	movl %edx,-228(%ebp)
	movl -228(%ebp),%esi
	negl %esi
	movl %esi,-32(%ebp)
	movl -220(%ebp),%eax
	movl %eax,-16(%ebp)
	.p2align 2,0x90
.L16:
	decl -32(%ebp)
	cmpl $0,-32(%ebp)
	jge .L19
	jmp .L17
	.p2align 2,0x90
.L19:
.stabn 68,0,39,.LM17-lu2_backsubstitute_sp__
.LM17:
	movl 16(%ebp),%edx
	movl %edx,-236(%ebp)
	movl -16(%ebp),%esi
	decl %esi
	movl %esi,-240(%ebp)
	movl -240(%ebp),%eax
	leal 0(,%eax,4),%eax
	movl %eax,-244(%ebp)
	movl -236(%ebp),%edx
	movl -244(%ebp),%esi
	flds (%esi,%edx)
	fstps -12(%ebp)
.stabn 68,0,40,.LM18-lu2_backsubstitute_sp__
.LM18:
.LBB6:
	movl 20(%ebp),%eax
	movl %eax,-248(%ebp)
	movl -16(%ebp),%edx
	incl %edx
	movl %edx,-252(%ebp)
	movl -248(%ebp),%esi
	movl (%esi),%eax
	subl -252(%ebp),%eax
	movl %eax,-256(%ebp)
	movl -256(%ebp),%edx
	incl %edx
	movl %edx,-28(%ebp)
	movl -252(%ebp),%esi
	movl %esi,-20(%ebp)
	.p2align 2,0x90
.L20:
	decl -28(%ebp)
	cmpl $0,-28(%ebp)
	jge .L23
	jmp .L21
	.p2align 2,0x90
.L23:
.stabn 68,0,41,.LM19-lu2_backsubstitute_sp__
.LM19:
	movl 8(%ebp),%eax
	movl %eax,-260(%ebp)
	movl -20(%ebp),%edx
	decl %edx
	movl %edx,-264(%ebp)
	movl -264(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-268(%ebp)
	movl -16(%ebp),%eax
	decl %eax
	movl %eax,-272(%ebp)
	movl -272(%ebp),%edx
	imull %ecx,%edx
	movl %edx,-276(%ebp)
	movl -268(%ebp),%esi
	addl -276(%ebp),%esi
	movl %esi,-280(%ebp)
	movl 16(%ebp),%eax
	movl %eax,-284(%ebp)
	movl -20(%ebp),%edx
	decl %edx
	movl %edx,-288(%ebp)
	movl -288(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-292(%ebp)
	movl -260(%ebp),%eax
	movl -280(%ebp),%edx
	flds (%edx,%eax)
	movl -284(%ebp),%esi
	movl -292(%ebp),%eax
	fmuls (%eax,%esi)
	flds -12(%ebp)
	fsubp %st,%st(1)
	fstps -12(%ebp)
.L24:
.stabn 68,0,42,.LM20-lu2_backsubstitute_sp__
.LM20:
.L22:
	incl -20(%ebp)
	jmp .L20
	.p2align 2,0x90
.L21:
.LBE6:
.stabn 68,0,43,.LM21-lu2_backsubstitute_sp__
.LM21:
	movl 16(%ebp),%edx
	movl %edx,-296(%ebp)
	movl -16(%ebp),%esi
	decl %esi
	movl %esi,-300(%ebp)
	movl -300(%ebp),%eax
	leal 0(,%eax,4),%eax
	movl %eax,-304(%ebp)
	movl 8(%ebp),%edx
	movl %edx,-308(%ebp)
	leal 4(%ecx),%esi
	movl %esi,-312(%ebp)
	movl -16(%ebp),%eax
	decl %eax
	movl %eax,-316(%ebp)
	movl -312(%ebp),%edx
	imull -316(%ebp),%edx
	movl %edx,-320(%ebp)
	flds -12(%ebp)
	movl -308(%ebp),%esi
	movl -320(%ebp),%eax
	fdivs (%eax,%esi)
	movl -296(%ebp),%edx
	movl -304(%ebp),%esi
	fstps (%esi,%edx)
.L25:
.stabn 68,0,44,.LM22-lu2_backsubstitute_sp__
.LM22:
.L18:
	decl -16(%ebp)
	jmp .L16
	.p2align 2,0x90
.L17:
.LBE5:
.stabn 68,0,45,.LM23-lu2_backsubstitute_sp__
.LM23:
	jmp .L2
.LBE2:
.stabn 68,0,45,.LM24-lu2_backsubstitute_sp__
.LM24:
.L2:
	leal -360(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
.Lfe1:
		.size		 lu2_backsubstitute_sp__,.Lfe1-lu2_backsubstitute_sp__
.stabs "ii:(0,20)",128,0,10,-4
.stabs "ll:(0,20)",128,0,10,-8
.stabs "sum:(0,32)",128,0,11,-12
.stabs "row:(0,20)",128,0,14,-16
.stabs "col:(0,20)",128,0,14,-20
.stabn 192,0,0,.LBB2-lu2_backsubstitute_sp__
.stabs "__g77_do_0:(0,20)",128,0,18,-24
.stabn 192,0,0,.LBB3-lu2_backsubstitute_sp__
.stabs "__g77_do_2:(0,20)",128,0,29,-32
.stabn 192,0,0,.LBB4-lu2_backsubstitute_sp__
.stabn 224,0,0,.LBE4-lu2_backsubstitute_sp__
.stabn 224,0,0,.LBE3-lu2_backsubstitute_sp__
.stabs "__g77_do_4:(0,20)",128,0,38,-32
.stabn 192,0,0,.LBB5-lu2_backsubstitute_sp__
.stabs "__g77_do_5:(0,20)",128,0,40,-28
.stabn 192,0,0,.LBB6-lu2_backsubstitute_sp__
.stabn 224,0,0,.LBE6-lu2_backsubstitute_sp__
.stabn 224,0,0,.LBE5-lu2_backsubstitute_sp__
.stabn 224,0,0,.LBE2-lu2_backsubstitute_sp__
.Lscope0:
.stabs "",36,0,0,.Lscope0-lu2_backsubstitute_sp__
	.text
	.stabs "",100,0,0,.Letext
.Letext:
	.ident	"GCC: (GNU) f77 2.95.4 20020320 [FreeBSD]"
