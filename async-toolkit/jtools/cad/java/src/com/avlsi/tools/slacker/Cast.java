package com.avlsi.tools.slacker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.csp.util.UniqueLabel;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.PortTypeInterface;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.HashCounter;
import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.StringUtil;

/**
 * Extract CAST connection information and feed into Slacker.
 **/
public class Cast {
    private final Slacker slacker;
    private final Map typeCache;

    /** If set, force top-level slacker_leaf=true, slacker_alignment=0 */
    static CellInterface topCell = null;

    /** Used to match cells that inherits leaf */
    private static UnaryPredicate leafMatcher = null;

    /** Constructor */
    public Cast(final Slacker slacker) {
        this.slacker = slacker;
        this.typeCache = new HashMap();
    }

    public static class SlackerDirective {
        private final CellInterface cell;

        public SlackerDirective(final CellInterface cell) {
            this.cell = cell;
        }

        public float getTime(final CellUtils.Channel castChan,
                             final float def) {
            return
                getFloatDirective(cell, castChan,
                                  DirectiveConstants.SLACKER_TIME,
                                  def);
        }

        public float getTime(final CellUtils.Channel castChan) {
            return getTime(castChan,
                           isLeaf() && castChan.getDirection() < 0 ? 1 : 0);
        }

        public float getFreeSlack(final CellUtils.Channel castChan,
                                  final float def) {
            return
                getFloatDirective(cell, castChan,
                                  DirectiveConstants.SLACKER_FREE_SLACK, def);
        }

        public float getFreeSlack(final CellUtils.Channel castChan) {
            return getFreeSlack(castChan, 0);
        }

        public int getInitialTokens(final CellUtils.Channel castChan,
                                    final int def) {
            return
                getIntDirective(cell, castChan,
                                DirectiveConstants.SLACKER_INITIAL_TOKENS, def);
        }

        public int getInitialTokens(final CellUtils.Channel castChan) {
            return getInitialTokens(castChan, 0);
        }

        public int getHandshakes(final CellUtils.Channel castChan,
                                 final int def) {
            return
                getIntDirective(cell, castChan,
                                DirectiveConstants.SLACKER_HANDSHAKES,
                                def);
        }

        public int getHandshakes(final CellUtils.Channel castChan) {
            return getHandshakes(
                    castChan, isLeaf() && castChan.getDirection() < 0 ? 1 : 0);
        }

        public int getAlignment(final CellUtils.Channel castChan,
                                final int def) {
            final Integer defAlignment = (Integer)
                DirectiveUtils.getTopLevelDirective(cell,
                        DirectiveConstants.SLACKER_ALIGNMENT);
            return
                getIntDirective(cell, castChan,
                                DirectiveConstants.SLACKER_ALIGNMENT,
                                defAlignment == null ? def : defAlignment);
        }

        public int getAlignment(final CellUtils.Channel castChan) {
            return getAlignment(castChan, -1);
        }

        public float getTransitions(final CellUtils.Channel castChan,
                                    final float def) {
            return
                getFloatDirective(cell, castChan,
                                  DirectiveConstants.SLACKER_TRANSITIONS, def);
        }

        public boolean isIgnore(final CellUtils.Channel castChan,
                                final boolean def) {
            return
                getBooleanDirective(cell, castChan,
                                    DirectiveConstants.SLACKER_IGNORE, def);
        }

        public boolean isIgnore(final CellUtils.Channel castChan) {
            return isIgnore(castChan, false);
        }

        public boolean isDontTouch(final CellUtils.Channel castChan,
                                   final boolean def) {
            return
                getBooleanDirective(cell, castChan,
                                    DirectiveConstants.SLACKER_DONT_TOUCH,
                                    def);
        }

        public boolean isDontTouch(final CellUtils.Channel castChan) {
            return isDontTouch(castChan, false);
        }

        public boolean isLeaf() {
            return Cast.isLeaf(cell);
        }

        public boolean isSlack() {
            return Cast.isSlack(cell);
        }
    }

    /** Should slacker consider this cell type a fragment? */
    private boolean isFragment(final CellInterface cell) {
        if (isLeaf(cell)) return false; // slacker_leaf overrides fragment
        return ((Boolean) DirectiveUtils.getTopLevelDirective(cell,
                            DirectiveConstants.FRAGMENT)).booleanValue()
            || ((Boolean) DirectiveUtils.getTopLevelDirective(cell,
                            DirectiveConstants.SYNCHRONOUS)).booleanValue();
    }

    /** Is this a slacker_leaf=true cell? */
    static boolean isLeaf(final CellInterface cell) {
        final Boolean val = (Boolean) DirectiveUtils.getTopLevelDirective(
                cell, DirectiveConstants.SLACKER_LEAF);
        boolean v = val == null ? false : val.booleanValue();
        if (cell==topCell) {
            if (v) v=false;
            else {
                System.err.println("ERROR: --check-leaf failed because slacker_leaf=false.");
                System.exit(1);
            }
        }
        return v;        
    }

    /** Is this a slacker_is_slack=true cell? */
    static boolean isSlack(final CellInterface cell) {
        return ((Boolean) DirectiveUtils.getTopLevelDirective(cell,
                            DirectiveConstants.SLACKER_IS_SLACK)).booleanValue();
    }

    /** Get CAST channel by instance and port name */
    private CellUtils.Channel getChannel(final HierName instance,
                                         final PortDefinition port) {
        return new CellUtils.Channel(instance, port.getName(), null,
                                     port.getType(), -1,
                                     port.getDirection());
    }

    private static Object getParameter(final CellUtils.Channel chan,
                                       final boolean hierName) {
        final String name = chan.getFullName();
        return hierName ? toHierName(name) : name;
    }

    private static Object getParameter(final CellUtils.Channel chan) {
        return getParameter(chan, false);
    }

    /** Get an integer directive on a port */
    private static int getIntDirective(final CellInterface cell,
                                       final CellUtils.Channel port,
                                       final String directive, final int def) {
        final Map map = DirectiveUtils.getTopLevelDirective(cell, directive, 
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);

        Integer value = (Integer) map.get(getParameter(port));
        if (value == null && port.getParent() != null) {
            value = (Integer) map.get(getParameter(port.getParent()));
        }

        return value == null ? def : value.intValue();
    }

    private static Pattern ARRAY_INDEX = Pattern.compile("(.*)\\[(.*)\\]$");
    /**
     * Try to interpret chan as part of a wide channel.  Returns the name of
     * chan with the last array index removed if possible.  This is necessary
     * for directives involving internal channels, because the CellUtil.Channel
     * objects created for them do not correctly set the parent.  On the other
     * hand, this should be harmless in other cases, as the parser had already
     * validated the directive parameter, so a non-wide channel array won't
     * appear in the directive map.
     **/
    private static String tryWide(final CellUtils.Channel chan) {
        final String name = chan.getFullName();
        final Matcher m = ARRAY_INDEX.matcher(name);
        if (m.matches()) {
            final String[] indices = StringUtil.split(m.group(2), ',');
            if (indices.length > 1) {
                final String[] partial = new String[indices.length - 1];
                System.arraycopy(indices, 0, partial, 0, indices.length - 1);
                return m.group(1) + '[' + StringUtil.join(partial, ',') + ']';
            } else {
                return m.group(1);
            }
        } else {
            return name;
        }
    }

    /** Get a float directive on a port */
    private static float getFloatDirective(final CellInterface cell,
                                           final CellUtils.Channel port,
                                           final String directive,
                                           final float def) {
        final Map map = DirectiveUtils.getTopLevelDirective(cell, directive, 
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);
        return getFloatDirective(map, port, def);
    }

    /** Get a float directive on a port */
    private static float getFloatDirective(final Map map,
                                           final CellUtils.Channel port,
                                           final float def) {
        Float value = (Float) map.get(getParameter(port));
        if (value == null) {
            if (port.getParent() == null) {
                value = (Float) map.get(tryWide(port));
            } else {
                value = (Float) map.get(getParameter(port.getParent()));
            }
        }

        return value == null ? def : value.floatValue();
    }

    /** Get a boolean directive from a directive map */
    private static boolean getBooleanDirective(final Map map,
                                               final CellUtils.Channel port,
                                               final boolean def) {
        Boolean value = (Boolean) map.get(getParameter(port));
        if (value == null) {
            if (port.getParent() == null) {
                value = (Boolean) map.get(tryWide(port));
            } else {
                value = (Boolean) map.get(getParameter(port.getParent()));
            }
        }

        return value == null ? def : value.booleanValue();
    }

    /** Get a boolean directive on a port */
    private static boolean getBooleanDirective(final CellInterface cell,
                                               final CellUtils.Channel port,
                                               final String directive,
                                               final boolean def) {
        final Map map = DirectiveUtils.getTopLevelDirective(cell, directive, 
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);
        return getBooleanDirective(map, port, def);
    }

    private static boolean inheritsLeaf(final CellInterface cell) {
        if (leafMatcher == null) {
            leafMatcher =
                CellUtils.getTypeMatcher(
                    Collections.singleton("standard.base.leaf"));
        }
        return leafMatcher.evaluate(cell);
    }

    /** Create and add a Slacker Channel */
    private Channel getChannel(final Type type,
                               final CellInterface cell,
                               final CellUtils.Channel castChan,
                               final AliasedSet/*<HierName>*/ locals,
                               final int passthru,
                               final boolean isPort) {
        // common channel information
        final boolean input = castChan.getDirection() < 0;
        final String name = castChan.getFullName();
        final String parent =
            castChan.getParent() != null ? castChan.getParent().getFullName()
                                         : null;
        final CellInterface chanCell = getSubcell(cell, toHierName(name));
        double cost = (Float) DirectiveUtils.getTopLevelDirective(chanCell,
                DirectiveConstants.SLACKER_COST);
        boolean chanDontTouch =
            (Boolean) DirectiveUtils.getTopLevelDirective(chanCell,
                DirectiveConstants.SLACKER_DONT_TOUCH);
        boolean leafOutput = type.isLeaf && !input;
        final SlackerDirective slackerDir = new SlackerDirective(cell);

        if (isPort) {
            // get channel directives on ports
            boolean ignore = slackerDir.isIgnore(castChan);
            float time = slackerDir.getTime(castChan, Float.NaN);
            if (Float.isNaN(time)) {
                if (type.isLeaf && !ignore && !input && !inheritsLeaf(cell)) {
                    // warn about implicit slacker_time directives in slacker
                    // leaf cells which do not <+ leaf
                    System.err.println("WARNING: " + type.name + "/" +
                                       castChan.getName() +
                                       " has no explicit slacker_time");
                }
                time = leafOutput ? 1 : 0;
            }
            float freeSlack = slackerDir.getFreeSlack(castChan);
            int initialTokens = slackerDir.getInitialTokens(castChan);
            int handshakes =
                slackerDir.getHandshakes(castChan, leafOutput ? 1 : 0);
            int alignment = cell == topCell ? 0 // force slacker_alignment=0
                                            : slackerDir.getAlignment(castChan);
            boolean dontTouch =  slackerDir.isDontTouch(castChan, chanDontTouch);
            float transitions = slackerDir.getTransitions(castChan, Float.NaN);

            // handle ignore-existing-slack option
            if (slackerDir.isSlack() && slacker.ignoreExistingSlack) {
                time = 0;
                freeSlack = 0;
                handshakes = 0;
            }

            // create and add Channel to Type
            Channel chan = new Channel(name, parent, passthru, time, freeSlack,
                                       cost, 0, handshakes, initialTokens,
                                       alignment, ignore, isPort, dontTouch);
            if (input) chan = type.addInPort(chan);
            else       chan = type.addOutPort(chan);
            return chan;
        }
        else {
            // create and add Channel to Type
            float freeSlack =
                getFloatDirective(
                        DirectiveUtils.getSubcellDirective(cell,
                            DirectiveConstants.SLACKER_FREE_SLACK,
                            DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE),
                        castChan, 0);
            boolean ignore = 
                getBooleanDirective(
                        DirectiveUtils.getSubcellDirective(cell,
                            DirectiveConstants.SLACKER_IGNORE,
                            DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE),
                        castChan, false);
            boolean defaultDontTouch = (Boolean)
                DirectiveUtils.getTopLevelDirective(cell,
                        DirectiveConstants.SLACKER_DONT_TOUCH);
            boolean dontTouch =
                getBooleanDirective(
                        DirectiveUtils.getSubcellDirective(cell,
                            DirectiveConstants.SLACKER_DONT_TOUCH,
                            DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE),
                        castChan, chanDontTouch || defaultDontTouch);

            // find forward latency due to extra_delay on data rails
            final Set rails = DirectiveUtils.getExplicitTrues(
                    DirectiveUtils.getTopLevelDirective(chanCell,
                        DirectiveConstants.SLACKER_USE_EXTRA_DELAY,
                        DirectiveConstants.NODE_TYPE));
            float latency = Float.NEGATIVE_INFINITY;
            final Map dir = DirectiveUtils.getSubcellDirective(cell,
                    DirectiveConstants.EXTRA_DELAY,
                    DirectiveConstants.HALFOP_TYPE);
            final Map ups = DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getUps(dir));
            final Map dns = DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getDowns(dir));
            for (Iterator i = rails.iterator(); i.hasNext(); ) {
                final HierName rail = (HierName) i.next();
                final HierName hn = HierName.append(toHierName(name), rail);
                final HierName canon =
                    (HierName) locals.getCanonicalKey(hn);
                Float up = (Float) ups.get(canon); // extra_delay on chan.d[i]+
                Float dn = (Float) dns.get(canon); // extra_delay on chan.d[i]-
                latency = Math.max(latency, up == null ? 0 : up);
                latency = Math.max(latency, dn == null ? 0 : dn);
            }
            // convert from DSim units to stages
            final float latencyStages =
                Float.isInfinite(latency) ? 0 : latency / 200;

            // create channel
            Channel chan = new Channel(name, parent, -1, 0, freeSlack, cost,
                                       latencyStages, 0, 0, -1, ignore, false,
                                       dontTouch);
            chan = type.addChannel(chan);
            return chan;
        }
    }

    /** Silly helper */
    private static HierName toHierName(final String s) {
        try {
            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("Cannot construct HierName: " + s, e);
        }
    }

    private static CellInterface getChannel(final CellInterface cell,
                                            final CellUtils.Channel chan) {
        final HierName name = toHierName(chan.getFullName()).head();
        return cell.getSubcell(name);
    }

    /** Create and add a local Slacker Channel with best canonical name */
    private Channel getChannel(final Type type,
                               final CellInterface cell,
                               final HierName instance,
                               final AliasedSet/*<HierName>*/ locals,
                               final AliasedSet narrowAliases,
                               final PortDefinition port) {
        final CellUtils.Channel target = getChannel(instance, port);
        final CellUtils.Channel canon =
            (CellUtils.Channel) narrowAliases.getCanonicalKey(target);
        if (canon == null) return null;

        // examine all aliased channels to find the best name to use
        CellUtils.Channel chan_local = null;
        CellUtils.Channel chan_port  = null;
        for (Iterator i = narrowAliases.getAliases(canon); i.hasNext(); ) {
            final CellUtils.Channel chan = (CellUtils.Channel) i.next();
            HierName name = toHierName(chan.getFullName()).head();
            final CellInterface subci = getChannel(cell, chan);
            
            // is the channel locally declared?
            if (subci != null && subci.isChannel()) {
                // if multiple locally declared channels are aliased together,
                // use the one that is lexicographically the smallest
                if (chan_local == null || chan.compareTo(chan_local) < 0)
                    chan_local = chan;
                if (cell.isPortSubcell(name) &&
                    (chan_port == null || chan.compareTo(chan_port) < 0))
                    chan_port = chan;
            }
        }

        // preferentially choose channel from port, local, canon
        final CellUtils.Channel source;
        if      (chan_port!=null)  source = chan_port;
        else if (chan_local!=null) source = chan_local;
        else                       source = canon;

        // return Channel with the best name
        return getChannel(type, cell, source, locals, -1, false);
    }

    private Iterable<PortDefinition> getPortDefinitions(
            final CellInterface cell) {
        return CollectionUtils.iterable(
                    CellUtils.flattenPorts(cell.getPortDefinitions(),
                                           CellUtils.CHANNEL_TYPE));
    }

    private static CellInterface getSubcell(final CellInterface cell,
                                            HierName name) {
        CellInterface curr = cell;
        while (curr != null) {
            Pair<HierName,HierName> p =
                CellUtils.getFirstInstance(curr, name, true);
            assert p.getFirst() != null;
            curr = curr.getSubcell(p.getFirst());
            if (p.getSecond() == null) {
                break;
            } else {
                name = p.getSecond();
            }
        }
        return curr;
    }

    private static CellUtils.Channel getChannel(final CellInterface cell,
                                                final HierName name) {
        final Pair<HierName,HierName> p =
            CellUtils.getFirstInstance(cell, name, true);
        final CellInterface subcell = cell.getSubcell(p.getFirst());
        final HierName inst, chan;
        if (subcell.isNode() || subcell.isChannel()) {
            inst = null;
            chan = name;
        } else {
            inst = p.getFirst();
            chan = p.getSecond();
        }
        return new CellUtils.Channel(inst, chan.getAsString('.'), null, null,
                                     -1, -1);
    }

    /** Create a new Slacker Type given a CellInterface */
    public Type process(final CellInterface cell) {
        final String typeName = cell.getFullyQualifiedType();
        final Type candidate = (Type) typeCache.get(typeName);
        if (candidate != null) return candidate;
        final Type type = new Type(cell.getFullyQualifiedType(),isLeaf(cell));

        final AliasedSet/*<CellUtils.Channel>*/ nodeAliases =
            new AliasedSet(new NaturalOrderComparator());
        final AliasedSet/*<CellUtils.Channel>*/ narrowAliases =
            new AliasedSet(new NaturalOrderComparator());

        // don't try to process a fragment
        if (isFragment(cell)) {
            System.err.println("WARNING: " + type.name + " is a fragment.");
            return null;
        }

        final CellUtils.ChannelConnectionPredicate
            nodeConnected = new CellUtils.ChannelConnectionPredicate() {
                private boolean isSlackerChannel(CellInterface cell) {
                    return (Boolean) DirectiveUtils.getTopLevelDirective(cell,
                            DirectiveConstants.SLACKER_CHANNEL);
                }

                private void getSlackerChannels(
                        HierName prefix, CellInterface cell,
                        List<Pair<HierName,CellInterface>> result) {
                    if (isSlackerChannel(cell)) {
                        result.add(new Pair<HierName,CellInterface>(prefix, cell));
                    } else {
                        for (Iterator i = cell.getPortSubcellPairs();
                                i.hasNext(); ) {
                            Pair<HierName,CellInterface> p =
                                (Pair<HierName,CellInterface>) i.next();
                            getSlackerChannels(
                                    HierName.append(prefix, p.getFirst()),
                                    p.getSecond(),
                                    result);
                        }
                    }
                }

                private List<String> getNodes(final CellInterface ci) {
                    final List<String> result = new ArrayList<>();
                    (new CellUtils.MarkPort() {
                        protected void mark(
                            final com.avlsi.fast.ports.NodeType nodeType,
                            final String name, final int direction) {
                            result.add(name);
                        }
                    }).mark(ci);
                    return result;
                }

                public boolean test(CellUtils.Channel out,
                                    CellUtils.Channel in,
                                    AliasedSet/*<Channel>*/ connections) {
                    final HierName outInst = toHierName(out.getFullName());
                    final CellInterface outCell = getSubcell(cell, outInst);
                    final HierName inInst = toHierName(in.getFullName());
                    final CellInterface inCell = getSubcell(cell, inInst);
                    final List<Pair<HierName,CellInterface>> outSlackerChans =
                        new ArrayList<>();
                    final List<Pair<HierName,CellInterface>> inSlackerChans =
                        new ArrayList<>();
                    getSlackerChannels(outInst, outCell, outSlackerChans);
                    getSlackerChannels(inInst, inCell, inSlackerChans);

                    final int outSize = outSlackerChans.size();
                    boolean connected = false;
                    if (outSize == inSlackerChans.size()) {
                        for (int i = 0; i < outSize; ++i) {
                            Pair<HierName,CellInterface> oc =
                                outSlackerChans.get(i);
                            Pair<HierName,CellInterface> ic =
                                inSlackerChans.get(i);
                            connected =
                                oc.getSecond() == ic.getSecond() &&
                                getNodes(oc.getSecond())
                                    .stream()
                                    .map(x -> toHierName(x))
                                    .allMatch(x ->
                                        connections.areEquivalent(
                                            getChannel(cell,
                                                HierName.append(
                                                    oc.getFirst(),
                                                    x)),
                                            getChannel(cell,
                                                HierName.append(
                                                    ic.getFirst(),
                                                    x))));
                            if (!connected) break;
                        }
                    }
                    return connected;
                }
            };

        // get channel information for this cell
        final AliasedSet/*<HierName>*/ locals =
            CellUtils.getChannelConnection(
                cell, null, null, null,
                nodeAliases, narrowAliases,
                new AliasedSet(new NaturalOrderComparator()),
                nodeConnected, null, null,
                new HashSet(), new HashSet(), Collections.EMPTY_MAP, true, false,
                new com.avlsi.tools.cadencize.Cadencize(false), null);

        // identify all e1of(N) channels
        final Map narrowChannels = new HashMap();
        for (Iterator i = nodeAliases.getCanonicalKeys(); i.hasNext(); ) {
            for (Iterator j = nodeAliases.getAliases(i.next()); j.hasNext(); ) {
                final CellUtils.Channel node = (CellUtils.Channel) j.next();
                if (node.getParent() != null)
                    narrowChannels.put(node.getParent(), node.getParent());
            }
        }

        // find ports that are aliased to determine passthru index
        final HashCounter aliasCounter = new HashCounter();
        for (PortDefinition port : getPortDefinitions(cell)) {
            final CellUtils.Channel target = getChannel(null, port);
            final CellUtils.Channel canon =
                (CellUtils.Channel) narrowAliases.getCanonicalKey(target);
            if (canon != null) aliasCounter.add(canon, 1);
        }

        // define input/output ports
        final UniqueLabel passthruLabel = new UniqueLabel();
        for (PortDefinition port : getPortDefinitions(cell)) {
            final CellUtils.Channel target = getChannel(null, port);
            final CellUtils.Channel match =
                (CellUtils.Channel) narrowChannels.get(target);
            final CellUtils.Channel canon =
                (CellUtils.Channel) narrowAliases.getCanonicalKey(target);
            final int passthru = isLeaf(cell) || 
                aliasCounter.getCount(canon) < 2 ? -1 :
                passthruLabel.getLabel(canon);
            final Channel chan =
                getChannel(type, cell, match, locals, passthru, true);
        }

        // process subcells
        if (!isLeaf(cell)) {
            // check for fragment subcells
            ArrayList fragmentSubcells = new ArrayList();
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subci = (CellInterface) p.getSecond();
                if (subci.isChannel() || subci.isNode()) continue;
                if (isFragment(subci))
                    fragmentSubcells.add(subci.getFullyQualifiedType());
            }
            // warn about fragment subcells but continue anyways (for BD)
            if (fragmentSubcells.size()>0) {
                System.err.println("WARNING: " + type.name + 
                                   " should set slacker_leaf=true\n" +
                                   "  or all of these should " + fragmentSubcells);
            }
            // warn about cell with no subcells
            if (!cell.containsSubcells()) {
                System.err.println("WARNING: " + type.name + 
                                   " should set slacker_leaf=true");
            }
            // process subcells
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subci = (CellInterface) p.getSecond();
                if (subci.isChannel() || subci.isNode()) continue;
                if (isFragment(subci)) continue; // skip fragment subcells
                
                final HierName instance = (HierName) p.getFirst();
                final Subcell subcell =
                    new Subcell(process(subci), instance.getAsString('.'));
                for (PortDefinition port : getPortDefinitions(subci)) {
                    final Channel chan =
                        getChannel(type, cell, instance, locals,
                                   narrowAliases, port);
                    if (chan==null)
                        System.err.println("WARNING: " + type.name + "/" +
                                           instance + "." + port.getName() + 
                                           " channel is mangled.");
                    else if (port.getDirection() == PortDefinition.IN) {
                        subcell.addInPort(chan);
                    } else if (port.getDirection() == PortDefinition.OUT) {
                        subcell.addOutPort(chan);
                    } else {
                        continue;
                    }
                }
                type.addSubcell(subcell);
            }
        }

        typeCache.put(typeName, type);
        slacker.addType(type);
        return type;
    }
}
