(defun DecideTemplate ( OutInvNW1 OutInvPW1 NandNW5 NandPW1 LeNW1 LePW1 )

	when((OutInvNW1<=1.56)&&(OutInvPW1<=2.08)&&(NandNW5<1.69)&&(NandPW1<0.65)&&(LeNW1<2.21)&&(LePW1<3.9)
		printf("Template\n")
		Output="Template"
		)

	when((OutInvNW1<=1.56)&&(OutInvPW1<=2.08)&&(NandNW5>=1.69)&&(NandPW1>=0.65)&&(LeNW1>=2.21)&&(LePW1>=3.9)
		printf("Template1\n")
		Output="Template1"
		)

	when((OutInvNW1>=1.56)&&(OutInvPW1>=2.08)
		printf("Template2\n")
		Output="Template2"
		)

	Output
	)


(defun Template ()
	(let (text fl k parameter1 parameter2 parameter3 parameter4 OutInvNW1 OutInvPW1 NandNW5 NandPW1 LeNW1 LePW1 OutPut)

	
	fl = infile("Parameter.txt")
	while( gets(k fl) != nil
  	text = parseString( k " \n")
	parameter1 = nth(0 text) 
	parameter2 = nth(1 text) 
	parameter3 = nth(2 text) 
	parameter4 = nth(3 text) 

	when((parameter1=="R.0")
		OutInvNW1=evalstring(parameter3)
		OutInvPW1=evalstring(parameter4)
		)
	when((parameter1=="rv")
		NandNW5=evalstring(parameter3)
		NandPW1=evalstring(parameter4)
		)
	when((parameter1=="L.e")
		LeNW1=evalstring(parameter3)
		LePW1=evalstring(parameter4)
		)
	)
	DecideTemplate( OutInvNW1 OutInvPW1 NandNW5 NandPW1 LeNW1 LePW1)
))

