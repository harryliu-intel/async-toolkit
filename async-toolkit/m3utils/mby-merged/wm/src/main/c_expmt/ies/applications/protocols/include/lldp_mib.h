/** vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_mib.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for LLDP database.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or 
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#ifndef LLDP_MIB_H
#define LLDP_MIB_H

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <pthread.h>

#include <lldp_int.h>
#include <lldp_list.h>
#include <lldp_event.h>

/** Port admin-state types. */
enum lldpAdminStatus {
    /** Port TX is enabled, RX is disabled */
    LLDP_ADMIN_TX_ONLY,

    /** Port TX is disabled, RX is enabled */
    LLDP_ADMIN_RX_ONLY,

    /** Port TX and RX are enabled */
    LLDP_ADMIN_TX_AND_RX,

    /** Port TX and RX are disabled */
    LLDP_ADMIN_DISABLED
};

/** Port TLV TX enable bit-maps. */
enum lldpTlvTxEnable {
    LLDP_TLV_ENABLE_PORT_DESC = (1 << 0),
    LLDP_TLV_ENABLE_SYS_NAME  = (1 << 1),
    LLDP_TLV_ENABLE_SYS_DESC  = (1 << 2),
    LLDP_TLV_ENABLE_SYS_CAP   = (1 << 3),
    LLDP_TLV_ENABLE_ALL       = LLDP_TLV_ENABLE_PORT_DESC |
                                LLDP_TLV_ENABLE_SYS_NAME  |
                                LLDP_TLV_ENABLE_SYS_DESC  |
                                LLDP_TLV_ENABLE_SYS_CAP
};

/** Chassis id types. */
enum lldpChassisIdSubtype {
    LLDP_CHASSIS_ID_CHASSIS_COMPONENT = 1,
    LLDP_CHASSIS_ID_INTERFACE_ALIAS   = 2,
    LLDP_CHASSIS_ID_PORT_COMPONENT    = 3,
    LLDP_CHASSIS_ID_MAC_ADDRESS       = 4,
    LLDP_CHASSIS_ID_NETWORK_ADDRESS   = 5,
    LLDP_CHASSIS_ID_INTERFACE_NAME    = 6,
    LLDP_CHASSIS_ID_LOCALLY_ASSIGNED  = 7 

};

/** Port id types. */
enum lldpPortIdSubtype {
    LLDP_PORT_ID_INTERFACE_ALIAS      = 1,
    LLDP_PORT_ID_PORT_COMPONENT       = 2,
    LLDP_PORT_ID_MAC_ADDRESS          = 3,
    LLDP_PORT_ID_NETWORK_ADDRESS      = 4,
    LLDP_PORT_ID_INTERFACE_NAME       = 5,
    LLDP_PORT_ID_AGENT_CIRCUIT_ID     = 6,
    LLDP_PORT_ID_LOCALLY_ASSIGNED     = 7

};

/** Management address id types. */
enum lldpManAddrIfSubtype {
    LLDP_MAN_ADDR_ID_UNKNOWN          = 1,
    LLDP_MAN_ADDR_ID_INTERFACE_INDEX  = 2,
    LLDP_MAN_ADDR_ID_SYSTEM_PORT_NUM  = 3
};

/** System capability bit-maps. */
enum lldpSystemCapability {
    LLDP_SYSTEM_CAPABILITY_OTHER      = 1,
    LLDP_SYSTEM_CAPABILITY_REPEATER   = 2,
    LLDP_SYSTEM_CAPABILITY_BRIDGE     = 4,
    LLDP_SYSTEM_CAPABILITY_WLAN       = 8,
    LLDP_SYSTEM_CAPABILITY_ROUTER     = 16,
    LLDP_SYSTEM_CAPABILITY_TELEPHONE  = 32,
    LLDP_SYSTEM_CAPABILITY_DOCSIS     = 64,
    LLDP_SYSTEM_CAPABILITY_STATION    = 128

};

/** LLDP RX state machine states. */
enum lldpRxState {
    LLDP_RX_STATE_UNKNOWN,

    LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL,
    LLDP_RX_STATE_DELETE_AGED_INFO,
    LLDP_RX_STATE_RX_LLDP_INITIALIZE,
    LLDP_RX_STATE_RX_WAIT_FOR_FRAME,
    LLDP_RX_STATE_RX_FRAME,
    LLDP_RX_STATE_DELETE_INFO,
    LLDP_RX_STATE_UPDATE_INFO
};

/** LLDP TX state machine states. */
enum lldpTxState {
    LLDP_TX_STATE_UNKNOWN,

    LLDP_TX_STATE_TX_LLDP_INITIALIZE,
    LLDP_TX_STATE_TX_IDLE,
    LLDP_TX_STATE_TX_INFO_FRAME,
    LLDP_TX_STATE_TX_SHUTDOWN_FRAME
};

#define LLDP_MAX_LOCAL_PORTS  4096
#define LLDP_MAX_REMOTE_PORTS 4096
#define LLDP_MAX_PACKET_SIZE  1500

#define LLDP_MAX_NEIGHBORS    4096

/*****************************************************************************
 * lldpMsap
 *****************************************************************************/

/** LLDP remote MSAP identifier. */
struct lldpMsap {
    enum lldpChassisIdSubtype lldpMsapChassisIdSubtype;
    char lldpMsapChassisId[256];
    size_t lldpMsapChassisIdLen;

    enum lldpPortIdSubtype lldpMsapPortIdSubtype;
    char lldpMsapPortId[256];
    size_t lldpMsapPortIdLen;

    /** additional indexing data */

    uint32_t lldpRemIndex;
    uint16_t lldpRemLocalPortNum;
};


/*****************************************************************************
 * lldpConfiguration
 *****************************************************************************/

/** LLDP Port Configuration Data */
struct lldpPortConfig {
    lldp_list         node;

    /* Index */

    /** The index value used to identify the port component
        (contained in the local chassis with the LLDP agent)
        associated with this entry.

        The value of this object is used as a port index to the
        lldpPortConfigTable. */
    uint16_t lldpPortConfigPortNum;

    /* Data */

    /** The administratively desired status of the local LLDP agent.

        If the associated lldpPortConfigAdminStatus object has a
        value of 'txOnly(1)', then LLDP agent will transmit LLDP
        frames on this port and it will not store any information
        about the remote systems connected.
     
        If the associated lldpPortConfigAdminStatus object has a
        value of 'rxOnly(2)', then the LLDP agent will receive,
        but it will not transmit LLDP frames on this port.

        If the associated lldpPortConfigAdminStatus object has a
        value of 'txAndRx(3)', then the LLDP agent will transmit
        and receive LLDP frames on this port.

        If the associated lldpPortConfigAdminStatus object has a
        value of 'disabled(4)', then LLDP agent will not transmit or
        receive LLDP frames on this port.  If there is remote systems
        information which is received on this port and stored in
        other tables, before the port's lldpPortConfigAdminStatus
        becomes disabled, then the information will naturally age out. */
    enum lldpAdminStatus lldpPortConfigAdminStatus;

    /** The lldpPortConfigNotificationEnable controls, on a per
        port basis,  whether or not notifications from the agent
        are enabled. The value true(1) means that notifications are
        enabled; the value false(2) means that they are not. */
    bool lldpPortConfigNotificationEnable;

    /** The lldpPortConfigTLVsTxEnable, defined as a bitmap,
        includes the basic set of LLDP TLVs whose transmission is
        allowed on the local LLDP agent by the network management.
        Each bit in the bitmap corresponds to a TLV type associated
        with a specific optional TLV.

        It should be noted that the organizationally-specific TLVs
        are excluded from the lldpTLVsTxEnable bitmap.
            
        LLDP Organization Specific Information Extension MIBs should
        have similar configuration object to control transmission
        of their organizationally defined TLVs.

        The bit 'portDesc(0)' indicates that LLDP agent should
        transmit 'Port Description TLV'.

        The bit 'sysName(1)' indicates that LLDP agent should transmit
        'System Name TLV'.

        The bit 'sysDesc(2)' indicates that LLDP agent should transmit
        'System Description TLV'.

        The bit 'sysCap(3)' indicates that LLDP agent should transmit
        'System Capabilities TLV'.

        There is no bit reserved for the management address TLV type
        since transmission of management address TLVs are controlled
        by another object, lldpConfigManAddrTable.

        The default value for lldpPortConfigTLVsTxEnable object is
        empty set, which means no enumerated values are set.

        The value of this object must be restored from non-volatile
        storage after a re-initialization of the management system. */
    uint32_t lldpPortConfigTLVsTxEnable;

    /** The lldpConfigManAddrPortsTxEnable, which is defined
        as a truth value and configured by the network management,
        determines whether the system management address instance
        will be transmitted on the ports. */
    bool lldpConfigManAddrPortsTxEnable;

    /* Data (Non-MIB) */

    /** Indicates if port is enabled (link-status up) */
    bool lldpPortConfigEnabled;
};

/** LLDP Global Configuration Data */
struct lldpConfiguration
{
    /** The interval at which LLDP frames are transmitted on
        behalf of this LLDP agent.

        The default value for lldpMessageTxInterval object is
        30 seconds.

        The value of this object must be restored from non-volatile
        storage after a re-initialization of the management system. */
    uint32_t lldpMessageTxInterval;

    /** The time-to-live value expressed as a multiple of the
        lldpMessageTxInterval object.  The actual time-to-live value
        used in LLDP frames, transmitted on behalf of this LLDP agent,
        can be expressed by the following formula: TTL = min(65535,
        (lldpMessageTxInterval * lldpMessageTxHoldMultiplier)) For
        example, if the value of lldpMessageTxInterval is '30', and
        the value of lldpMessageTxHoldMultiplier is '4', then the
        value '120' is encoded in the TTL field in the LLDP header.

        The default value for lldpMessageTxHoldMultiplier object is 4.

        The value of this object must be restored from non-volatile
        storage after a re-initialization of the management system. */
    uint32_t lldpMessageTxHoldMultiplier;

    /** The lldpReinitDelay indicates the delay (in units of
        seconds) from when lldpPortConfigAdminStatus object of a
        particular port becomes 'disabled' until re-initialization
        will be attempted.

        The default value for lldpReintDelay object is two seconds.

        The value of this object must be restored from non-volatile
        storage after a re-initialization of the management system. */
    uint32_t lldpReinitDelay;

    /** The lldpTxDelay indicates the delay (in units
        of seconds) between successive LLDP frame transmissions 
        initiated by value/status changes in the LLDP local systems
        MIB.  The recommended value for the lldpTxDelay is set by the
        following formula :

        1 <= lldpTxDelay <= (0.25 * lldpMessageTxInterval)

        The default value for lldpTxDelay object is two seconds.

        The value of this object must be restored from non-volatile
        storage after a re-initialization of the management system. */
    uint32_t lldpTxDelay;

    /** This object controls the transmission of LLDP notifications.
        the agent must not generate more than one lldpRemTablesChange
        notification-event in the indicated period, where a
        'notification-event' is the transmission of a single
        notification PDU type to a list of notification destinations.
        If additional changes in lldpRemoteSystemsData object
        groups occur within the indicated throttling period,
        then these trap- events must be suppressed by the
        agent. An NMS should periodically check the value of
        lldpStatsRemTableLastChangeTime to detect any missed
        lldpRemTablesChange notification-events, e.g. due to
        throttling or transmission loss.

        If notification transmission is enabled for particular ports,
        the suggested default throttling period is 5 seconds.

        The value of this object must be restored from non-volatile
        storage after a re-initialization of the management system. */
    uint32_t lldpNotificationInterval;

    /** The table that controls LLDP frame transmission on individual
        ports. */
    struct lldpPortConfig *lldpPortConfig[LLDP_MAX_LOCAL_PORTS];
    lldp_list lldpPortConfigTable;

    /* Data (Non-MIB) */

    /** Local chassis MAC address */
    uint64_t chassisMacAddress;
};


/*****************************************************************************
 * lldpStatistics
 *****************************************************************************/

/** LLDP Port Statistics Data */
struct lldpStatsPort {
    lldp_list         node;

    /* Index */

    /** The index value used to identify the port component
        (contained in the local chassis with the LLDP agent)
        associated with this entry.

        The value of this object is used as a port index to the
        lldpStatsPortTable. */
    uint16_t lldpStatsPortNum;

    /** Data (RX) */

    /** The number of LLDP frames received by this LLDP agent on
        the indicated port, and then discarded for any reason.
        This counter can provide an indication that LLDP header
        formating problems may exist with the local LLDP agent in
        the sending system or that LLDPDU validation problems may
        exist with the local LLDP agent in the receiving system. */
    uint32_t lldpStatsRxPortFramesDiscardedTotal;

    /** The number of invalid LLDP frames received by this LLDP
        agent on the indicated port, while this LLDP agent is enabled. */
    uint32_t lldpStatsRxPortFramesErrors;

    /** The number of valid LLDP frames received by this LLDP agent
        on the indicated port, while this LLDP agent is enabled. */
    uint32_t lldpStatsRxPortFramesTotal;

    /** The number of LLDP TLVs discarded for any reason by this LLDP
        agent on the indicated port. */
    uint32_t lldpStatsRxPortTLVsDiscardedTotal;

    /** The number of LLDP TLVs received on the given port that
        are not recognized by this LLDP agent on the indicated port.
        
        An unrecognized TLV is referred to as the TLV whose type value
        is in the range of reserved TLV types (000 1001 - 111 1110)
        in Table 9.1 of IEEE Std 802.1AB-2004.  An unrecognized
        TLV may be a basic management TLV from a later LLDP version. */
    uint32_t lldpStatsRxPortTLVsUnrecognizedTotal;

    /** The counter that represents the number of age-outs that
        occurred on a given port.  An age-out is the number of
        times the complete set of information advertised by a
        particular MSAP has been deleted from tables contained in
        lldpRemoteSystemsData and lldpExtensions objects because
        the information timeliness interval has expired.

        This counter is similar to lldpStatsRemTablesAgeouts, except
        that the counter is on a per port basis.  This enables NMS to
        poll tables associated with the lldpRemoteSystemsData objects
        and all LLDP extension objects associated with remote systems
        on the indicated port only.

        This counter should be set to zero during agent initialization
        and its value should not be saved in non-volatile storage.
        When a port's admin status changes from 'disabled' to
        'rxOnly', 'txOnly' or 'txAndRx', the counter associated with
        the same port should reset to 0.  The agent should also flush
        all remote system information associated with the same port.

        This counter should be incremented only once when the
        complete set of information is invalidated (aged out) from
        all related tables on a particular port.  Partial aging
        is not allowed, and thus, should not change the value of
        this counter. */
    uint32_t lldpStatsRxPortAgeoutsTotal;

    /** Data (TX) */

    /** The number of LLDP frames transmitted by this LLDP agent
        on the indicated port. */
    uint32_t lldpStatsTxPortFramesTotal;
};

/** LLDP Global Statistics Data */
struct lldpStatistics {
    /** The value of sysUpTime object (defined in IETF RFC 3418)
        at the time an entry is created, modified, or deleted in the
        in tables associated with the lldpRemoteSystemsData objects
        and all LLDP extension objects associated with remote systems.

        An NMS can use this object to reduce polling of the
        lldpRemoteSystemsData objects. */
    uint32_t lldpStatsRemTablesLastChangeTime;

    /** The number of times the complete set of information
        advertised by a particular MSAP has been inserted into tables
        contained in lldpRemoteSystemsData and lldpExtensions objects.

        The complete set of information received from a particular
        MSAP should be inserted into related tables.  If partial
        information cannot be inserted for a reason such as lack
        of resources, all of the complete set of information should
        be removed.

        This counter should be incremented only once after the
        complete set of information is successfully recorded
        in all related tables.  Any failures during inserting
        information set which result in deletion of previously
        inserted information should not trigger any changes in
        lldpStatsRemTablesInserts since the insert is not completed
        yet or or in lldpStatsRemTablesDeletes, since the deletion
        would only be a partial deletion. If the failure was the
        result of lack of resources, the lldpStatsRemTablesDrops
        counter should be incremented once. */
    uint32_t lldpStatsRemTablesInserts;

    /** The number of times the complete set of information
        advertised by a particular MSAP has been deleted from
        tables contained in lldpRemoteSystemsData and lldpExtensions
        objects.

        This counter should be incremented only once when the
        complete set of information is completely deleted from all
        related tables.  Partial deletions, such as deletion of
        rows associated with a particular MSAP from some tables,
        but not from all tables are not allowed, thus should not
        change the value of this counter. */
    uint32_t lldpStatsRemTablesDeletes;

    /** The number of times the complete set of information
        advertised by a particular MSAP could not be entered into
        tables contained in lldpRemoteSystemsData and lldpExtensions
        objects because of insufficient resources. */
    uint32_t lldpStatsRemTablesDrops;

    /** The number of times the complete set of information
        advertised by a particular MSAP has been deleted from tables
        contained in lldpRemoteSystemsData and lldpExtensions objects
        because the information timeliness interval has expired.

        This counter should be incremented only once when the complete
        set of information is completely invalidated (aged out)
        from all related tables.  Partial aging, similar to deletion
        case, is not allowed, and thus, should not change the value
        of this counter. */
    uint32_t lldpStatsRemTablesAgeouts;

    /** A table containing LLDP transmission / reception statistics for
        individual ports.  Entries are not required to exist in
        this table while the lldpPortConfigEntry object is equal to
        'disabled(4)'. */
    struct lldpStatsPort *lldpStatsPort[LLDP_MAX_LOCAL_PORTS];
    lldp_list lldpStatsPortTable;
};


/*****************************************************************************
 * lldpLocalSystemData
 *****************************************************************************/

/** LLDP Port Local Data */
struct lldpLocPort {
   lldp_list         node;

    /* Index */

    /** The index value used to identify the port component
        (contained in the local chassis with the LLDP agent)
        associated with this entry.

        The value of this object is used as a port index to the
        lldpLocPortTable. */
    uint16_t lldpLocPortNum;

    /* Data */

    /** The type of port identifier encoding used in the associated
        'lldpLocPortId' object. */
    enum lldpPortIdSubtype lldpLocPortIdSubtype;

    /** The string value used to identify the port component
        associated with a given port in the local system. */
    char lldpLocPortId[256];

    /** portId Length */
    size_t lldpLocPortIdLen;

    /** The string value used to identify the 802 LAN station's port
        description associated with the local system.  If the local
        agent supports IETF RFC 2863, lldpLocPortDesc object should
        have the same value of ifDescr object. */
    char lldpLocPortDesc[256];

    /* Data (Non-MIB) */

    /** Rx State Machine */
    enum lldpRxState rxState;
    enum lldpRxState rxStateLast;

    /** RX State */
    bool     rxInfoAged;
    bool     rcvFrame;
    bool     badFrame;

    /** Tx State Machine */
    enum lldpTxState txState;
    enum lldpTxState txStateLast;

    /** TX State */
    uint32_t txTTL;
    uint32_t txTTR;
    uint32_t txDelayWhile;
    uint32_t txShutdownWhile;

    /** Entry change flags (by local or by remote) */
    bool     somethingChangedRemote;
    bool     somethingChangedLocal;

    /** Neighbors status */
    uint32_t portNeighbors;
    uint32_t mgmtNeighbors;

    bool     tooManyNeighbors;
    uint32_t tooManyNeighborsTimer;
};

/** LLDP Port Local Management-Address Data */
struct lldpLocManAddr {
   lldp_list         node;

    /* Index */

    /** The type of management address identifier encoding used in
        the associated 'lldpLocManagmentAddr' object. */
    uint32_t lldpLocManAddrSubtype;

    /** The string value used to identify the management address
        component associated with the local system.  The purpose of
        this address is to contact the management entity. */
    char lldpLocManAddr[32];

    /* Data */

    /** The total length of the management address subtype and the
        management address fields in LLDPDUs transmitted by the
        local LLDP agent.

        The management address length field is needed so that the
        receiving systems that do not implement SNMP will not be
        required to implement an iana family numbers/address length
        equivalency table in order to decode the management address. */
    uint32_t lldpLocManAddrLen;

    /** The enumeration value that identifies the interface numbering
        method used for defining the interface number, associated
        with the local system. */
    enum lldpManAddrIfSubtype lldpLocManAddrIfSubtype;

    /** The integer value used to identify the interface number
        regarding the management address component associated with
        the local system. */
    uint32_t lldpLocManAddrIfId;

    /** The OID value used to identify the type of hardware component
        or protocol entity associated with the management address
        advertised by the local system agent. */
    char lldpLocManAddrOID[256];
};

/** LLDP Global Local Data */
struct lldpLocalSystemData {
    /** The type of encoding used to identify the chassis
        associated with the local system. */
    enum lldpChassisIdSubtype lldpLocChassisIdSubtype;

    /** The string value used to identify the chassis component
        associated with the local system. */
    char lldpLocChassisId[256];

    /** chassisId Length */
    size_t lldpLocChassisIdLen;

    /** The string value used to identify the system name of the
        local system. If the local agent supports IETF RFC 3418,
        lldpLocSysName object should have the same value of sysName
        object. */
    char lldpLocSysName[256];

    /** The string value used to identify the system description
        of the local system.  If the local agent supports IETF RFC 3418,
        lldpLocSysDesc object should have the same value of sysDesc
        object. */
    char lldpLocSysDesc[256];

    /** The bitmap value used to identify which system capabilities
        are supported on the local system. */
    uint16_t lldpLocSysCapSupported;

    /** The bitmap value used to identify which system capabilities
        are enabled on the local system. */
    uint16_t lldpLocSysCapEnabled;

    /** This table contains one or more rows per port information
        associated with the local system known to this agent. */
    struct lldpLocPort *lldpLocPort[LLDP_MAX_LOCAL_PORTS];
    lldp_list lldpLocPortTable;

    /** This table contains management address information on the
        local system known to this agent. */
    lldp_list lldpLocManAddrTable;
};


/*****************************************************************************
 * lldpRemoteSystemsData
 *****************************************************************************/

/** LLDP Port Remote Management-Address Data */
struct lldpRemManAddr {
   lldp_list         node;

    /* Index */

    /** The type of management address identifier encoding used in
        the associated 'lldpRemManagmentAddr' object. */
    int lldpRemManAddrSubtype;

    /** The string value used to identify the management address
        component associated with the remote system.  The purpose
        of this address is to contact the management entity. */
    char lldpRemManAddr[32];

    /* Data */

    /** The enumeration value that identifies the interface numbering
        method used for defining the interface number, associated
        with the remote system. */
    int lldpRemManAddrIfSubtype;

    /** The integer value used to identify the interface number
        regarding the management address component associated with
        the remote system. */
    uint32_t lldpRemManAddrIfId;

    /** The OID value used to identify the type of hardware component
        or protocol entity associated with the management address
        advertised by the remote system agent. */
    char lldpRemManAddrOID[256];
};

/** LLDP Port Remote Data */
struct lldpRem {
   lldp_list         node;

    /* Index */

    /** This object represents an arbitrary local integer value used
        by this agent to identify a particular connection instance,
        unique only for the indicated remote system.

        An agent is encouraged to assign monotonically increasing
        index values to new entries, starting with one, after each
        reboot.  It is considered unlikely that the lldpRemIndex
        will wrap between reboots. */
    uint32_t lldpRemIndex;

    /* Data */

    /** A TimeFilter for this entry.  See the TimeFilter textual
        convention in IETF RFC 2021 and 
        http://www.ietf.org/IESG/Implementations/RFC2021-Implementation.txt
        to see how TimeFilter works. */
    uint32_t lldpRemTimeMark;

    /** The index value used to identify the port component
        (contained in the local chassis with the LLDP agent)
        associated with this entry.  The lldpRemLocalPortNum
        identifies the port on which the remote system information
        is received.

        The value of this object is used as a port index to the
        lldpRemTable. */
    uint16_t lldpRemLocalPortNum;

    /** The type of encoding used to identify the chassis associated
        with the remote system. */
    enum lldpChassisIdSubtype lldpRemChassisIdSubtype;

    /** The string value used to identify the chassis component
        associated with the remote system. */
    char lldpRemChassisId[256];

    /** chassisId Length */
    size_t lldpRemChassisIdLen;

    /** The type of port identifier encoding used in the associated
        'lldpRemPortId' object. */
    enum lldpPortIdSubtype lldpRemPortIdSubtype;

    /** The string value used to identify the port component
        associated with the remote system. */
    char lldpRemPortId[256];

    /** portId Length */
    size_t lldpRemPortIdLen;

    /** The string value used to identify the description of
        the given port associated with the remote system. */
    char lldpRemPortDesc[256];

    /** The string value used to identify the system name of the
        remote system. */
    char lldpRemSysName[256];

    /** The string value used to identify the system description
        of the remote system. */
    char lldpRemSysDesc[256];

    /** The bitmap value used to identify which system capabilities
        are supported on the remote system. */
    uint16_t lldpRemSysCapSupported;

    /** The bitmap value used to identify which system capabilities
        are enabled on the remote system. */
    uint16_t lldpRemSysCapEnabled;

    /** This table contains one or more rows per management address
        information on the remote system learned on a particular port
        contained in the local chassis known to this agent. */
    lldp_list lldpRemManAddrTable;

    /** remote mac address */
    uint64_t lldpRemMacAddress;

    /* Data (Non-MIB) */

    /** Remote data expiration time. */
    uint32_t          rxInfoTTL;
};

/** LLDP Global Remote Data */
struct lldpRemoteSystemsData {
    /** This table contains one or more rows per physical network
        connection known to this agent.  The agent may wish to ensure
        that only one lldpRemEntry is present for each local port,
        or it may choose to maintain multiple lldpRemEntries for
        the same local port.

        The following procedure may be used to retrieve remote
        systems information updates from an LLDP agent:

          1. NMS polls all tables associated with remote systems
             and keeps a local copy of the information retrieved.
             NMS polls periodically the values of the following
             objects:
                a. lldpStatsRemTablesInserts
                b. lldpStatsRemTablesDeletes
                c. lldpStatsRemTablesDrops
                d. lldpStatsRemTablesAgeouts
                e. lldpStatsRxPortAgeoutsTotal for all ports.

          2. LLDP agent updates remote systems MIB objects, and
             sends out notifications to a list of notification
             destinations.

          3. NMS receives the notifications and compares the new
             values of objects listed in step 1.  

             Periodically, NMS should poll the object
             lldpStatsRemTablesLastChangeTime to find out if anything
             has changed since the last poll.  if something has
             changed, NMS will poll the objects listed in step 1 to
             figure out what kind of changes occurred in the tables.

             if value of lldpStatsRemTablesInserts has changed,
             then NMS will walk all tables by employing TimeFilter
             with the last-polled time value.  This request will
             return new objects or objects whose values are updated
             since the last poll.

             if value of lldpStatsRemTablesAgeouts has changed,
             then NMS will walk the lldpStatsRxPortAgeoutsTotal and
             compare the new values with previously recorded ones.
             For ports whose lldpStatsRxPortAgeoutsTotal value is
             greater than the recorded value, NMS will have to
             retrieve objects associated with those ports from
             table(s) without employing a TimeFilter (which is
             performed by specifying 0 for the TimeFilter.)

             lldpStatsRemTablesDeletes and lldpStatsRemTablesDrops
             objects are provided for informational purposes. */
    struct lldpRem *lldpRem[LLDP_MAX_REMOTE_PORTS];
    lldp_list lldpRemTable;
};


/*****************************************************************************
 * lldpObjects
 *****************************************************************************/

struct lldpProtocolIf;
struct lldpPhysicalIf;

/** LLDP Database Objects */
struct lldpObjects {
    /** LLDP configuration */
    struct lldpConfiguration     lldpConfiguration;

    /** LLDP statistics */
    struct lldpStatistics        lldpStatistics;

    /** LLDP local data */
    struct lldpLocalSystemData   lldpLocalSystemData;

    /** LLDP remote data */
    struct lldpRemoteSystemsData lldpRemoteSystemsData;

    /** Interface to upper level protocol such as DCBX. */
    struct lldpProtocolIf       *protocol;

    /** Interface to low level physical layer. */
    struct lldpPhysicalIf       *physical;

    /** Database lock */
    pthread_mutex_t              lock;
    uint16_t                     remoteIndex;

    /** State-machine thread id */
    pthread_t                    thread;

    /** State-machine Event-queue */
    lldp_eventQueue              queue;

    /** State-machine exit indication */
    bool                         shutdown;
};


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

extern struct lldpObjects *lldpDB;


/*****************************************************************************
 * Data Management Functions 
 *****************************************************************************/

/** Create LLDP Database */
struct lldpObjects    *lldpObjectsCreate();

/** Delete LLDP Database */
int                    lldpObjectsDestroy(struct lldpObjects *obj);

/** Create Port Configuration Entry */
struct lldpPortConfig *lldpPortConfigCreate(int portNum);

/** Delete Port Configuration Entry */
int                    lldpPortConfigDestroy(struct lldpPortConfig *obj);

/** Get Port Configuration Entry by port number */
struct lldpPortConfig *lldpPortConfigGet(int portNum);

/** Create Local Port Entry */
struct lldpLocPort    *lldpLocPortCreate(int portNum, enum lldpPortIdSubtype portIdSubtype, const char *portId, size_t portIdLen, const char *portDesc);

/** Delete Local Port Entry */
int                    lldpLocPortDestroy(struct lldpLocPort *obj);

/** Get Local Port Entry by port number */
struct lldpLocPort    *lldpLocPortGet(int portNum);

/** Create Local Port Management-Address Entry */
struct lldpLocManAddr *lldpLocManAddrCreate(uint32_t addrSubtype, const char *addr);

/** Delete Local Port Management-Address Entry */
int                    lldpLocManAddrDestroy(struct lldpLocManAddr *obj);

/** Get Local Port Management-Address Entry by address */
struct lldpLocManAddr *lldpLocManAddrGet(const char *addr);

/** Create Local Port Statistics Entry */
struct lldpStatsPort  *lldpStatsPortCreate(int portNum);

/** Delete Local Port Statistics Entry */
int                    lldpStatsPortDestroy(struct lldpStatsPort *obj);

/** Get Local Port Statistics Entry by port number */
struct lldpStatsPort  *lldpStatsPortGet(int portNum);

/** Create Remote Port Entry */
struct lldpRem        *lldpRemCreate(int portNum, const struct lldpMsap *msap);

/** Delete Remote Port Entry */
int                    lldpRemDestroy(struct lldpRem *obj);

/** Get Remote Port Entry from RemoteIndex */
struct lldpRem        *lldpRemGetByIndex(int remoteIndex);

/** Get Remote Port Entry from MSAP */
struct lldpRem        *lldpRemGetByMsap(const struct lldpMsap *msap);

/** Create Remote Port Management-Address Entry */
struct lldpRemManAddr *lldpRemManAddrCreate(struct lldpRem *remPort, uint32_t addrSubtype, const char *addr);

/** Delete Remote Port Management-Address Entry */
int                    lldpRemManAddrDestroy(struct lldpRemManAddr *obj);

/** Get Remote Port Management-Address Entry from Remote-Port and Address */
struct lldpRemManAddr *lldpRemManAddrGet(const struct lldpRem *remPort, const char *addr);

/** Get Remote Port Management-Address Entry from Remote-Index and Address */
struct lldpRemManAddr *lldpRemManAddrGetByIndex(int remoteIndex, const char *addr);

/** Get Remote Port Management-Address Entry from MSAP and Address */
struct lldpRemManAddr *lldpRemManAddrGetByMsap(const struct lldpMsap *msap, const char *addr);

/** Create MSAP from Remote Port */
struct lldpMsap       *lldpMsapFromRemPort(struct lldpMsap *msap, const struct lldpRem *remPort);

/** Delete MSAP from Remote Port */
void                   lldpMsapClear(struct lldpMsap *msap);


/*****************************************************************************
 * Helper Functions 
 *****************************************************************************/

/** Format Chassis-Id string */
char *lldpChassisIdString(int chassisIdSubtype, char *chassisId, size_t chassisIdLen, char *buf, size_t len);

/** Format Port-Id string */
char *lldpPortIdString(int portIdSubtype, char *portId, size_t portIdLen, char *buf, size_t len);

#endif /** LLDP_MIB_H */
