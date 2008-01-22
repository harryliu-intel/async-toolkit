	.file	"mul_mtransposem_sp.f"
	.version	"01.01"
gcc2_compiled.:
.text
	.p2align 2,0x90
.globl mul_mtransposem_sp__
		.type		 mul_mtransposem_sp__,@function
mul_mtransposem_sp__:
	pushl %ebp
	movl %esp,%ebp
	subl $284,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 24(%ebp),%esi
	movl %esi,-28(%ebp)
	movl -28(%ebp),%edi
	movl (%edi),%eax
	movl %eax,-32(%ebp)
	movl -32(%ebp),%esi
	sall $5,%esi
	movl %esi,-36(%ebp)
	movl -36(%ebp),%edx
	leal 0(,%eax,4),%ecx
	movl 20(%ebp),%edi
	movl %edi,-40(%ebp)
	movl -40(%ebp),%esi
	movl (%esi),%ebx
	movl %edx,%edi
	imull %ebx,%edi
	movl %edi,-268(%ebp)
	movl %ecx,%esi
	imull %ebx,%esi
	movl %esi,-272(%ebp)
	movl 28(%ebp),%edi
	movl %edi,-48(%ebp)
	movl -48(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-44(%ebp)
	movl -44(%ebp),%edi
	movl %edi,-56(%ebp)
	movl -56(%ebp),%esi
	sall $5,%esi
	movl %esi,-60(%ebp)
	movl -60(%ebp),%edi
	movl %edi,-52(%ebp)
	movl -44(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-64(%ebp)
	movl 20(%ebp),%edi
	movl %edi,-72(%ebp)
	movl -72(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-68(%ebp)
	movl -52(%ebp),%edi
	imull -68(%ebp),%edi
	movl %edi,-76(%ebp)
	movl -64(%ebp),%esi
	imull -68(%ebp),%esi
	movl %esi,-80(%ebp)
	movl 28(%ebp),%edi
	movl %edi,-88(%ebp)
	movl -88(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-84(%ebp)
	movl -84(%ebp),%edi
	movl %edi,-96(%ebp)
	movl -96(%ebp),%esi
	sall $5,%esi
	movl %esi,-100(%ebp)
	movl -100(%ebp),%edi
	movl %edi,-92(%ebp)
	movl -84(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-104(%ebp)
	movl 24(%ebp),%edi
	movl %edi,-112(%ebp)
	movl -112(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-108(%ebp)
	movl -92(%ebp),%edi
	imull -108(%ebp),%edi
	movl %edi,-116(%ebp)
	movl -104(%ebp),%esi
	imull -108(%ebp),%esi
	movl %esi,-120(%ebp)
	movl 24(%ebp),%edi
	movl %edi,-124(%ebp)
	movl -124(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-128(%ebp)
	movl -128(%ebp),%edi
	movl %edi,-16(%ebp)
	movl $1,-4(%ebp)
	.p2align 2,0x90
.L3:
	decl -16(%ebp)
	cmpl $0,-16(%ebp)
	jge .L6
	jmp .L4
	.p2align 2,0x90
.L6:
	movl 28(%ebp),%esi
	movl %esi,-132(%ebp)
	movl -132(%ebp),%edi
	movl (%edi),%edi
	movl %edi,-136(%ebp)
	movl -136(%ebp),%esi
	movl %esi,-20(%ebp)
	movl $1,-8(%ebp)
	.p2align 2,0x90
.L7:
	decl -20(%ebp)
	cmpl $0,-20(%ebp)
	jge .L10
	jmp .L17
	.p2align 2,0x90
.L10:
	movl 16(%ebp),%edi
	movl %edi,-140(%ebp)
	movl -8(%ebp),%esi
	decl %esi
	movl %esi,-144(%ebp)
	movl -144(%ebp),%edi
	leal 0(,%edi,4),%edi
	movl %edi,-148(%ebp)
	movl -4(%ebp),%esi
	decl %esi
	movl %esi,-152(%ebp)
	movl -152(%ebp),%edi
	imull -104(%ebp),%edi
	movl %edi,-156(%ebp)
	movl -148(%ebp),%esi
	addl -156(%ebp),%esi
	movl %esi,-160(%ebp)
	movl -140(%ebp),%edi
	movl -160(%ebp),%esi
	movl $0,(%esi,%edi)
	movl 20(%ebp),%edi
	movl %edi,-164(%ebp)
	movl -164(%ebp),%esi
	movl (%esi),%esi
	movl %esi,-168(%ebp)
	movl -168(%ebp),%edi
	movl %edi,-24(%ebp)
	movl $1,-12(%ebp)
	.p2align 2,0x90
.L11:
	decl -24(%ebp)
	cmpl $0,-24(%ebp)
	jge .L14
	jmp .L16
	.p2align 2,0x90
.L14:
	movl 16(%ebp),%esi
	movl %esi,-172(%ebp)
	movl -8(%ebp),%edi
	decl %edi
	movl %edi,-176(%ebp)
	movl -176(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-180(%ebp)
	movl -4(%ebp),%edi
	decl %edi
	movl %edi,-184(%ebp)
	movl -184(%ebp),%esi
	imull -104(%ebp),%esi
	movl %esi,-188(%ebp)
	movl -180(%ebp),%edi
	addl -188(%ebp),%edi
	movl %edi,-192(%ebp)
	movl 16(%ebp),%esi
	movl %esi,-196(%ebp)
	movl -8(%ebp),%edi
	decl %edi
	movl %edi,-200(%ebp)
	movl -200(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-204(%ebp)
	movl -4(%ebp),%edi
	decl %edi
	movl %edi,-208(%ebp)
	movl -208(%ebp),%esi
	imull -104(%ebp),%esi
	movl %esi,-212(%ebp)
	movl -204(%ebp),%edi
	addl -212(%ebp),%edi
	movl %edi,-216(%ebp)
	movl 8(%ebp),%esi
	movl %esi,-220(%ebp)
	movl -4(%ebp),%edi
	decl %edi
	movl %edi,-224(%ebp)
	movl -224(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-228(%ebp)
	movl -12(%ebp),%edi
	decl %edi
	movl %edi,-232(%ebp)
	movl -232(%ebp),%esi
	imull %ecx,%esi
	movl %esi,-236(%ebp)
	movl -228(%ebp),%edi
	addl -236(%ebp),%edi
	movl %edi,-240(%ebp)
	movl 12(%ebp),%esi
	movl %esi,-244(%ebp)
	movl -8(%ebp),%edi
	decl %edi
	movl %edi,-248(%ebp)
	movl -248(%ebp),%esi
	leal 0(,%esi,4),%esi
	movl %esi,-252(%ebp)
	movl -12(%ebp),%edi
	decl %edi
	movl %edi,-256(%ebp)
	movl -256(%ebp),%esi
	imull -64(%ebp),%esi
	movl %esi,-260(%ebp)
	movl -252(%ebp),%edi
	addl -260(%ebp),%edi
	movl %edi,-264(%ebp)
	movl -220(%ebp),%esi
	movl -240(%ebp),%edi
	flds (%edi,%esi)
	movl -244(%ebp),%esi
	movl -264(%ebp),%edi
	fmuls (%edi,%esi)
	movl -196(%ebp),%esi
	movl -216(%ebp),%edi
	flds (%edi,%esi)
	faddp %st,%st(1)
	movl -172(%ebp),%esi
	movl -192(%ebp),%edi
	fstps (%edi,%esi)
.L15:
	nop
.L13:
	incl -12(%ebp)
	jmp .L11
	.p2align 2,0x90
.L12:
.L16:
	nop
.L9:
	incl -8(%ebp)
	jmp .L7
	.p2align 2,0x90
.L8:
.L17:
	nop
.L5:
	incl -4(%ebp)
	jmp .L3
	.p2align 2,0x90
.L4:
	jmp .L2
.L2:
	leal -296(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
.Lfe1:
		.size		 mul_mtransposem_sp__,.Lfe1-mul_mtransposem_sp__
	.ident	"GCC: (GNU) f77 2.95.4 20020320 [FreeBSD]"
