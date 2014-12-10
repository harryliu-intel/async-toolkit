package com.avlsi.tools.difflayout;

import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

public class DiffLayer {
    public static BufferedImage diff(GDSIIFile a, GDSIIFile b,
                                     int layer, int width) {
        LayerInfo info = LayerInfo.get(layer);

        int minx = Math.min(a.minx, b.minx);
        int miny = Math.min(a.miny, b.miny);
        int maxx = Math.min(a.maxx, b.maxx);
        int maxy = Math.min(a.maxy, b.maxy);

        int cellwidth = (maxx - minx);
        int cellheight = (maxy - miny);

        double scale = (width / (double)cellwidth);
        int height = (int)(cellheight * scale);
        int imgheight = height + 50;

        BufferedImage bi = new BufferedImage(width, imgheight,
                                             BufferedImage.TYPE_INT_RGB);
        Graphics2D g = bi.createGraphics();
        g.setBackground(Color.black);
        g.clearRect(0, 0, width, imgheight);
        g.setColor(Color.yellow);
        g.fillRect(0, 0, width, (imgheight - height));

        Font regular = g.getFont();
        Font bigger = regular.deriveFont(Font.BOLD, (float)36);
        g.setFont(bigger);
        FontMetrics fm = g.getFontMetrics();
        int textX = 1 + (width - fm.stringWidth(info.name)) / 2;
        int textY = (((imgheight - height) - fm.getAscent()) / 2 +
                     fm.getAscent());
        g.setColor(Color.black);
        g.drawString(info.name, textX, textY);

        g.translate(0, imgheight - height);
        g.scale(scale, -scale);
        g.translate(0, -maxy);
        g.addRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                               RenderingHints.VALUE_ANTIALIAS_ON));
        g.addRenderingHints(new RenderingHints(RenderingHints.KEY_TEXT_ANTIALIASING,
                                               RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
        
        Collection agons = a.getPolygonsForLayer(layer);
        Collection bgons = b.getPolygonsForLayer(layer);
        Collection same = PolygonUtils.samePolygons(agons, bgons);
        Collection diff = PolygonUtils.differentPolygons(agons, bgons);

        if (diff.size() == 0)
            return null;

        g.setColor(info.color.darker());
        PolygonUtils.drawPolygons(same, g);
        g.setColor(info.color);
        PolygonUtils.drawPolygons(diff, g);

        return bi;
    }
}
