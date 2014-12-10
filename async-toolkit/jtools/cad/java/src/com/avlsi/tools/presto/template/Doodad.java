/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import java.util.BitSet;
import com.avlsi.tools.presto.ChannelName;
import java.util.Map;
import com.avlsi.tools.presto.complete.Node;
import com.avlsi.tools.presto.output.Section;

/**
 * Doodad is a weird little class which is used as the "cookie" for each
 * Node when building the completion tree.  Doodad keeps track of the
 * necessary information for giving each generated node in the completion
 * tree a proper name and putting it in the proper section.  The
 * conjugate() static method can be called from a NodeBuilder, given
 * to TreeMunger.mungeTree(), to properly tag all the generated nodes in
 * the tree.
 */

public class Doodad {
    public static final int IN = 1;
    public static final int OUT = 2;
    public static final int BOTH = 3;
    public static final int EN = -1;
    
    public final Section section;
    public final int sectType;
    private final ChannelName chname;
    private final BitSet railsUsed;
    private final int railsPossible;
    
    public Doodad(Section section, int sectType, ChannelName chname,
                  BitSet railsUsed, int railsPossible) {
        this.section = section;
        this.sectType = sectType;
        this.chname = chname;
        this.railsUsed = railsUsed;
        this.railsPossible = railsPossible;
    }

    /**
     * @param type The type specified to buildNode(): NOR or CELEMENT
     * @param tier The tier specified to buildNode()
     * @param children The child nodes specified to buildNode()
     * @param temporaryNames Map(String, int[]): For each temporary name,
     *                       maps to a one-element array which is the next
     *                       array index to use.  As a special hack, the
     *                       array can be zero-length, in which case no
     *                       index will be used.
     * @param nameOverride   If non-null, this will be used as the name
     *                       for the new node
     * @return the newly constructed node, which buildNode() should return
     */
    public static Node conjugate(int type, int tier, Node[] children,
                                 Map temporaryNames, String nameOverride,
                                 int[] indexOverride) {
        assert (children.length > 0);
        Doodad[] doodads = new Doodad[children.length];
        for (int i = 0; i < children.length; i++)
            doodads[i] = (Doodad) children[i].getCookie();

        // Determine what section new node should go in
        int t = 0;
        for (int i = 0; i < doodads.length; i++)
            t |= doodads[i].sectType;

        Section newSect;
        String sectName;
        switch (t) {
        case IN:
            newSect = SectionFactory.mkInputSection(tier);
            sectName = "in";
            break;
        case OUT:
            newSect = SectionFactory.mkOutputTreeSection(tier);
            sectName = "out";
            break;
        case EN:
            newSect = SectionFactory.mkEnableSection(tier);
            sectName = "done";
            break;
        default:
            assert (t == BOTH);
            newSect = SectionFactory.mkAckSection(tier);
            sectName = "ack";
            break;
        }

        // Determine what name of new node should be
        ChannelName chname = doodads[0].chname;
        BitSet railsUsed = new BitSet();
        int railsPossible = doodads[0].railsPossible;
        for (int i = 0; i < doodads.length; i++) {
            if (chname != doodads[i].chname)
                chname = null;
            railsUsed.or(doodads[i].railsUsed);
            assert (railsPossible == doodads[i].railsPossible ||
                    chname == null);
        }

        String name;
        int[] arrayIndices;
        if (nameOverride != null) {
            name = nameOverride;
            arrayIndices = indexOverride;
        } else if (chname == null || (children.length == 1 &&
                                      children[0].getRail() == Node.NO_RAIL)) {
            name = sectName + tier;
            arrayIndices = (int[]) temporaryNames.get(name);
            if (arrayIndices == null)
                arrayIndices = new int[] { 0 };
            if (arrayIndices.length > 0)
                temporaryNames.put(name, new int[] { arrayIndices[0] + 1 });
        } else {
            StringBuffer buf = new StringBuffer(chname.getName());
            while (buf.charAt(0) == '_')
                buf.deleteCharAt(0); // hack to avoid extra underscores
            assert (railsUsed.cardinality() <= railsPossible);
            if (railsUsed.cardinality() == railsPossible) {
                buf.append('v');
            } else {
                for(int i = railsUsed.nextSetBit(0); i >= 0;
                    i = railsUsed.nextSetBit(i+1)) {
                    if (i > 9)
                        buf.append('_');
                    buf.append(Integer.toString(i));
                }
            }
            name = buf.toString();
            arrayIndices = chname.getArrayIndices();
        }

        if ((tier & 1) == 1)
            name = "_" + name;

        Doodad doodad = new Doodad(newSect, t, chname,
                                   railsUsed, railsPossible);
        return new Node(type, name, arrayIndices, tier, doodad, children);
    }
}
