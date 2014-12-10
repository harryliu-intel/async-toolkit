;out = outfile("/home/user/vjain/alpine/bus.list")

(defun busold ()
             (let (view shapes)
                    view = (geGetEditCellView)
	out = outfile("/home/user/vjain/alpine/bus.list")
      (foreach shape view->shapes
	(when (shape->layerName!="OBS")
         (when (shape->purpose=="bus")
	;out = outfile("/home/user/vjain/alpine/bus.list")
           Pattern=(dbGetPropByName shape "BusPattern")->value
           Prefix=(dbGetPropByName shape "BusPrefix")->value
           Pin=(dbGetPropByName shape "BusPin")->value
           FlipX=(dbGetPropByName shape "BusFlipX")->value
           FlipY=(dbGetPropByName shape "BusFlipY")->value
           StartVias=(dbGetPropByName shape "BusStartVias")->value
           EndVias=(dbGetPropByName shape "BusEndVias")->value
           ArrayPitch=((dbGetPropByName shape "BusArrayPitch")->value)/1.846
           DoubleVias=(dbGetPropByName shape "BusDoubleVias")->value
           Layer=(shape->layerName)
           boundingBox=(shape->bBox)
           cord=(shape->points)
           Width=(shape->width)/2
           bends=(shape->nPoints)
		fprintf(out "%s\n" Layer)
		fprintf(out "list(")
	(foreach point shape->points
		point1=((car point)/1.846)
		point2=((cadr point)/1.846)
		fprintf(out "%.2f:%.2f " point1 point2)
	)
		fprintf(out ")\n")
		;fprintf(out "%s\n" Layer)
		fprintf(out "bBox ")
	(foreach box shape->bBox
		box1=((car box)/1.846)
		box2=((cadr box)/1.846)
		fprintf(out "%.2f %.2f " box1 box2)
		)
		fprintf(out "\n")
  fprintf(out "%s \n" Pattern)
  fprintf(out "%s \n" Prefix)
  fprintf(out "%s \n" Pin)
  fprintf(out "%s \n" FlipX)
  fprintf(out "%s \n" FlipY)
  fprintf(out "%s \n" StartVias)
  fprintf(out "%s \n" EndVias)
  fprintf(out "%.2f \n" ArrayPitch)
  fprintf(out "%s \n" DoubleVias)
  fprintf(out "%.2f \n" Width)
  fprintf(out "%d \n" bends)
  print(Pattern)
  print(Prefix)
  print(Pin)
  print(FlipX)
  print(FlipY)
  print(StartVias)
  print(EndVias)
  print(ArrayPitch)
  print(DoubleVias)
  print(Layer)
  print(boundingBox)
  print(cord)
  print(Width)
  print(bends)
         ))
	(when (shape->layerName=="OBS")
	MTWO=(dbGetPropByName shape "M2")->value
	MTHREE=(dbGetPropByName shape "M3")->value
	MFOUR=(dbGetPropByName shape "M4")->value
	MFIVE=(dbGetPropByName shape "M5")->value
	MSIX=(dbGetPropByName shape "M6")->value
	MSEVEN=(dbGetPropByName shape "M7")->value
	MONE=(dbGetPropByName shape "M1")->value
        Layer=(shape->layerName)
	fprintf(out "%s\n" Layer)
		fprintf(out "list(")
	(foreach point shape->points
		point1=((car point)/1.846)
		point2=((cadr point)/1.846)
		fprintf(out "%.2f:%.2f " point1 point2)
	)
		fprintf(out ")\n")
        ;Layer=(shape->layerName)
	;fprintf(out "%s \n" Layer)
		fprintf(out "list(")
	(foreach box shape->bBox
		box1=((car box)/1.846)
		box2=((cadr box)/1.846)
		fprintf(out "%.2f:%.2f " box1 box2)
		)
		fprintf(out ")\n")
        ;Layer=(shape->layerName)
	;fprintf(out "%s \n" Layer)
	fprintf(out "%s \n" MTWO)
	fprintf(out "%s \n" MTHREE)
	fprintf(out "%s \n" MFOUR)
	fprintf(out "%s \n" MFIVE)
	fprintf(out "%s \n" MSIX)
	fprintf(out "%s \n" MSEVEN)
	fprintf(out "%s \n" MONE)
	fprintf(out "%s \n" MONE)
	fprintf(out "%s \n" MONE)
	fprintf(out "%s \n" MONE)
	fprintf(out "%s \n" MONE)



	)
 
)
)
  close(out)
)
