package com.avlsi.tools.difflayout;

import java.util.ArrayList;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import javax.imageio.ImageIO;
import java.io.InputStream;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.awt.image.Raster;
import java.util.TreeSet;

public class DiffLayers {
    public static void main(String[] argv) throws Exception {
        if (argv.length != 3) {
            System.err.println("Usage: DiffLayers file1.xml file2.xml outfile.png");
            System.exit(1);
        }

        InputStream file1 = new FileInputStream(argv[0]);
        InputStream file2 = new FileInputStream(argv[1]);
        File out = new File(argv[2]);

        GDSIIFile a = new GDSIIFile();
        a.parseXml(file1);
        GDSIIFile b = new GDSIIFile();
        b.parseXml(file2);

        TreeSet layers = new TreeSet();
        layers.addAll(a.getLayers());
        layers.addAll(b.getLayers());

        LinkedHashSet layersICareAbout = new LinkedHashSet();
        layersICareAbout.add(new Integer(37));
        layersICareAbout.add(new Integer(36));
        layersICareAbout.add(new Integer(35));
        layersICareAbout.add(new Integer(34));
        layersICareAbout.add(new Integer(33));
        layersICareAbout.add(new Integer(32));
        layersICareAbout.add(new Integer(31));
        layersICareAbout.add(new Integer(17));

        layersICareAbout.retainAll(layers);

        ArrayList images = new ArrayList();
        int height = 0;
        final int width = 800;

        for (Iterator it = layersICareAbout.iterator(); it.hasNext(); ) {
            Number layer = (Number) it.next();
            BufferedImage image = DiffLayer.diff(a, b, layer.intValue(), width);
            if (image != null) {
                images.add(image);
                height += image.getHeight();
            }
        }

        BufferedImage bi = new BufferedImage(width, height,
                                             BufferedImage.TYPE_INT_RGB);
        height = 0;
        for (Iterator it = images.iterator(); it.hasNext(); ) {
            BufferedImage image = (BufferedImage) it.next();
            Raster r = image.getData();
            r = r.createTranslatedChild(0, height);
            bi.setData(r);
            height += image.getHeight();
        }

        ImageIO.write(bi, "png", out);
    }
}
