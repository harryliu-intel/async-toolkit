/**
 * Copyright (C) 2005-2009 Fulcrum Microsystems. All rights reserved.
 * Unauthorized disclosure prohibited.
 */

package com.fulcrummicro.hw.verification.lib.testcase;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Specifies the type of reset that a class is interested in.
 *
 * @author zloh
 */
public class ResetType {

    /**************************************************************************
     * Protected Variables
     **************************************************************************/

    /** The set of reset domains this reset type listens to. */
    protected final HashSet<ResetDomain> domains;

    /** The set of reset domains for which the {@code port} argument is valid. */
    protected static final Set<ResetDomain> portDomains;

    static {
        // Initialize the set of port based reset domains.
        // [Note: Classes are initialized in textual order (see also
        // "The Java(TM) Virtual Machine Specification" Section 2.17.4). To
        // prevent null pointer exceptions when initializing a ResetType object,
        // this static initialization block MUST be placed at the top of the
        // file.]
        ResetDomain[] domains = { ResetDomain.EPL,
                                  ResetDomain.TB_EFT_RX,
                                  ResetDomain.TB_EFT_TX };
        portDomains = new HashSet<ResetDomain>(Arrays.asList(domains));
    }

    /**
     * The port this {@code ResetType} instance belongs to. Only used if the
     * reset domain is set to (i) the EPL reset domain or (ii) the testbench EFT
     * reset domain. If {@code port} equals -1, this {@code ResetType} instance
     * belongs to all ports, while if {@code port} equals 0 this ResetType
     * should not include EPL/EFT resets.
     */
    protected final int port;

    /**************************************************************************
     * Public Variables
     **************************************************************************/

    /** Enumeration of all possible reset domains */
    public enum ResetDomain {
        // The set of reset domains supported by the chip.
        CHIP,
        DEVICE,
        MASTER,
        COLD,
        PCIE_WARM,
        PCIE_HOT,
        PCIE_STICKY,
        PCIE_DATAPATH,
        EPL,
        SWITCH,
        IEEE1588,

        MASTER_MEM,
        COLD_MEM,
        PCIE_WARM_MEM,
        PCIE_HOT_MEM,
        PCIE_DATAPATH_MEM,
        EPL_MEM,
        SWITCH_MEM,
        
        // Obsolete reset domains.
        @Deprecated
        FH,
        @Deprecated
        LSM,
        @Deprecated
        EBI,

        /** Fatal error reset domain. For internal use by the testbench. */
        FATAL_ERROR,

        /** EBI bus abort reset domain. For internal use by the testbench. */
        EBI_BUS_ABORT,

        /**
         * Ethernet Frame Transactor (EFT) receiver reset domain. For internal
         * use by the testbench.
         */
        TB_EFT_RX,

        /**
         * Ethernet Frame Transactor (EFT) transmitter reset domain. For
         * internal use by the testbench.
         */
        TB_EFT_TX;
    }

    final static public ResetDomain[] EMPTY_DOMAIN = {};

    public static final ResetDomain[] CHIP_DOMAIN = {ResetDomain.CHIP};
    public static final ResetDomain[] FATAL_DOMAIN = {ResetDomain.FATAL_ERROR};

    public static final ResetDomain[] EBI_BUS_ABORT_DOMAIN = { ResetDomain.EBI_BUS_ABORT };

    public static final ResetDomain[] TB_EFT_RX_DOMAIN = { ResetDomain.TB_EFT_RX };

    public static final ResetDomain[] TB_EFT_TX_DOMAIN = { ResetDomain.TB_EFT_TX };

    public static final ResetType CHIP_RESET_TYPE = new ResetType(CHIP_DOMAIN);

    public static final ResetType FATAL_TYPE = new ResetType(FATAL_DOMAIN);

    /**
     * The {@code ResetType} object that should be returned by
     * {@link ResetEventListener#getType()} to indicate that the listener is not
     * interested in reset events.
     */
    public static final ResetType EMPTY_RESET_TYPE = new ResetType(EMPTY_DOMAIN);

    /**************************************************************************
     * Public Methods
     **************************************************************************/

    /**
     * Initializes a newly created {@code ResetType} object.
     *
     * @param domains
     *            is the set of reset domains this reset type should listen to.
     * @param port
     *            is the port this reset type should belong to.
     * @see ResetType#port
     */
    public ResetType(ResetDomain[] domains, int port) {
        this.domains = new HashSet<ResetDomain>(Arrays.asList(domains));
        this.port = IsPortDomain(this.domains) ? port : 0;
    }

    /**
     * Initializes a newly created {@code ResetType} object.
     * <p>
     * <strong>Note:</strong> For port based reset domains use
     * {@link ResetType#ResetType(ResetDomain[], int)}.
     * </p>
     *
     * @param domains
     *            is the set of reset domains this reset type should listen to.
     */
    public ResetType(ResetDomain[] domains) {
        this.domains = new HashSet<ResetDomain>(Arrays.asList(domains));
        if (IsPortDomain(this.domains)) {
            throw new IllegalArgumentException("port based reset domains unsupported");
        }
        this.port = 0;
    }

    /**
     * Returns <tt>true</tt> if this reset type contains ANY of the (reset
     * domain, port) tuples contained in the specified reset type.
     *
     * @param t
     *            is the reset type to compare against.
     * @return <tt>true</tt> if this reset type contains any of the (reset
     *         domain, port) tuples contained in the specified reset type.
     */
    public boolean contains(ResetType t) {
        boolean isIn;
        boolean isSubset = false;

        for (ResetDomain domain : t.domains) {
        	isIn = this.domains.contains(domain);
        	if (isIn && IsPortDomain(domain)) {
        		isIn &= (this.port == -1 || t.port == -1 || this.port == t.port);
        	}
        	isSubset |= isIn;
        }
        return isSubset;
    }

    /**
     * Returns <tt>true</tt> if this reset type listens to the specified reset
     * domain.
     *
     * @param domain
     *            is the reset domain whose listening status is to be checked.
     *            ResetDomain that we want to check
     * @return <tt>true</tt> if this reset type listens to the specified reset
     *         domain, <tt>false</tt> otherwise.
     */
    public boolean containsDomain(ResetDomain domain) {
        return this.domains.contains(domain);
    }

    /**
     * Retrieves the set of reset domains this reset type listens to.
     *
     * @return the set of reset domains this reset type listens to.
     */
    @SuppressWarnings("unchecked")
    public Set<ResetDomain> getDomains() {
        return (Set<ResetDomain>) this.domains.clone();
    }

    /**
     * Retrieve the port this reset type belongs to. Do not call this method if
     * this reset type does not listen to any port based reset domain.
     *
     * @return the port this reset type belongs to or <tt>-1</tt> if it belongs
     *         to all ports.
     */
    public int getPort() {
        assert IsPortDomain(this.domains) : "doesn't listen to port based reset domains";
        return this.port;
    }

    /**
     * Returns <tt>true</tt> if this reset type does not listen to any reset
     * domains.
     *
     * @return <tt>true</tt> if this reset type does not listen to any reset
     *         domains.
     */
    public boolean isEmptyDomain() {
        return this.domains.size() == 0 || this.domains.contains(EMPTY_DOMAIN);
    }

    public String toString() {
        StringBuilder buffer = new StringBuilder();

        buffer.append("ResetDomains[");
        for (ResetDomain domain : this.domains) {
            buffer.append(" " + domain);
            if (IsPortDomain(domain)) {
                buffer.append(String.format("(port:%d)", this.port));
            }
        }
        buffer.append(" ]");
        return buffer.toString();
    }

    /**************************************************************************
     * Private Methods
     **************************************************************************/

    private static boolean IsPortDomain(Set<ResetDomain> domains) {
        boolean isPortDomain = false;

        for (ResetDomain domain : domains) {
            isPortDomain |= IsPortDomain(domain);
        }
        return isPortDomain;
    }

    private static boolean IsPortDomain(ResetDomain domain) {
        return portDomains.contains(domain);
    }
}
