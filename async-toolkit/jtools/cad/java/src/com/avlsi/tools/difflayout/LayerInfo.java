package com.avlsi.tools.difflayout;

import java.awt.Color;

public class LayerInfo {
    public final Color color;
    public final String name;

    private LayerInfo(Color color, String name) {
        this.color = color;
        this.name = name;
    }

    public static LayerInfo get(int layer) {
        switch (layer) {
        case 17: return new LayerInfo(Color.red, "poly");
        case 30: return new LayerInfo(Color.gray, "contacts");

        case 31: return new LayerInfo(Color.cyan, "metal1");
        case 32: return new LayerInfo(Color.green, "metal2");
        case 33: return new LayerInfo(Color.magenta, "metal3");
        case 34: return new LayerInfo(Color.blue, "metal4");
        case 35: return new LayerInfo(Color.orange, "metal5");
        case 36: return new LayerInfo(Color.blue.darker(), "metal6");
        case 37: return new LayerInfo(Color.orange.darker(), "metal7");
        case 38: return new LayerInfo(Color.green.darker(), "metal8");

        case 51: return new LayerInfo(Color.cyan, "metal1-2 vias");
        case 52: return new LayerInfo(Color.green, "metal2-3 vias");
        case 53: return new LayerInfo(Color.magenta, "metal3-4 vias");
        case 54: return new LayerInfo(Color.blue, "metal4-5 vias");
        case 55: return new LayerInfo(Color.orange, "metal5-6 vias");
        case 56: return new LayerInfo(Color.blue.darker(), "metal6-7 vias");
        case 57: return new LayerInfo(Color.orange.darker(), "metal7-8 vias");

        case 131: return new LayerInfo(Color.cyan, "metal1 pins");
        case 132: return new LayerInfo(Color.green, "metal2 pins");
        case 133: return new LayerInfo(Color.magenta, "metal3 pins");
        case 134: return new LayerInfo(Color.blue, "metal4 pins");
        case 135: return new LayerInfo(Color.orange, "metal5 pins");
        case 136: return new LayerInfo(Color.blue.darker(), "metal6 pins");
        case 137: return new LayerInfo(Color.orange.darker(), "metal7 pins");
        case 138: return new LayerInfo(Color.green.darker(), "metal8 pins");
        }
        return new LayerInfo(Color.white, "unknown layer " + layer);
    }
}
