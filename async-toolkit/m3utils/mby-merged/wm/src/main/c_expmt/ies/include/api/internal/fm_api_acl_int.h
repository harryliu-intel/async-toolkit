/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_acl_int.h
 * Creation Date:   2005
 * Description:     Structures and functions for dealing with ACLs
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_ACL_INT_H
#define __FM_FM_API_ACL_INT_H


#define FM_TAKE_ACL_LOCK(sw)                                        \
    fmCaptureLock(&fmRootApi->fmSwitchStateTable[(sw)]->aclLock, \
                  FM_WAIT_FOREVER)

#define FM_DROP_ACL_LOCK(sw) \
    fmReleaseLock(&fmRootApi->fmSwitchStateTable[(sw)]->aclLock)

#define FM_ACL_PORTSET_MASK        0xffffffff

/* 
 *  Key position used to identify each mapper entry being added to the ACL
 *  mapper tree.
 */ 
#define FM_MAPPER_TYPE_KEY_POS             48
#define FM_MAPPER_L4_PROT_MAP_KEY_POS      32
#define FM_MAPPER_L4_PORT_START_KEY_POS    16
#define FM_MAPPER_L4_PORT_END_KEY_POS       0

#define FM_MAPPER_IP_LENGTH_START_KEY_POS  16
#define FM_MAPPER_IP_LENGTH_END_KEY_POS     0

#define FM_MAPPER_IP_ADDR_V6_KEY_POS       49


/*
 * A rule is a hit-condition with an associated action. 
 *  
 * The API allocates a structure of this type for each ACL rule defined,
 * and stores the pointer to the object in the 'rules' tree of the fm_acl
 * structure for the ACL to which the rule belongs.
 */ 
typedef struct
{
    /* Bitmask of the conditions to match on. */
    fm_aclCondition cond;

    /* Structure defining the values associated with the conditions. */
    fm_aclValue     value;

    /* Bitmask of the actions for this rule. */
    fm_aclActionExt action;

    /* Structure defining the parameters associated with the actions. */
    fm_aclParamExt  param;

    /* implementation-specific info for a particular switch type.
     * (In reality, this is used by FM2000 to store trigger information,
     * and is not used at all by FM4000.) */
    fm_int privateData;

    /* keeps track of associated ports in the ACL rule, where each bit position
     * is port number. This bit array is filled automatically when the port set
     * condition is used when the ACL rule is added */
    fm_bitArray associatedPorts;

    /* entry state */
    fm_aclEntryState state;

} fm_aclRule;


/* Structure to hold incremental ACL configuration */
typedef struct
{
    /* Bitmask of the conditions to support. */
    fm_aclCondition supportedCondition;

    /* Structure defining the mask associated with the conditions. */
    fm_aclValue     supportedConditionMask;

    /* Maximum number of action to perfom on this incremental ACL */
    fm_uint32       numAction;

} fm_aclIncremental;

/*
 * An ACL is a set of rules associated with a set of ports. 
 *  
 * The API allocates a structure of this type for each ACL defined, and 
 * stores the pointer to the object in the 'acls' tree of the fm_aclInfo 
 * structure for the switch to which the ACL belongs. 
 */ 
typedef struct
{
    /* maps rule number to fm_aclRule */
    fm_tree     rules;

    /* Tree which contains added rules between apply */
    fm_tree     addedRules;

    /* Tree which contains removed rules between apply */
    fm_tree     removedRules;

    /* Number of ports per type */
    fm_int      numberOfPorts[FM_ACL_TYPE_MAX];

    /* keeps track of associated ports and types, where each bit position
     * is type + (port * FM_ACL_TYPE_MAX) */
    fm_bitArray associatedPorts;

    /* a bitmask of FM_ACL_SCENARIO_*, indicating when this ACL is valid */
    fm_uint32   scenarios;

    /* the ACL precedence level (0 to FM_MAX_ACL_PRECEDENCE) */
    fm_int      precedence;

    /* the ACL mode (FM_ACL_MODE_INCREMENTAL or FM_ACL_MODE_STANDARD) */
    fm_int     aclMode;

    /* the ACL incremental predefined configuration */
    fm_aclIncremental aclIncremental;

    /* the ACL table selection */
    fm_aclTable aclTable;

    /* the ACL configuration refered by FM_ACL_KEEP_UNUSED_KEYS */
    fm_bool     aclKeepUnusedKeys;

} fm_acl;

/*
 * The API allocates a structure of this type for each ACL port set defined, and 
 * stores the pointer to the object in the 'portSet' tree of the fm_aclInfo 
 * structure for the switch to which the port set belongs. 
 */ 
typedef struct
{
    /* keeps track of associated ports in the port set, where each bit position
     * is port number */
    fm_bitArray associatedPorts;

    fm_byte     mappedValue;

} fm_aclPortSet;

/* macros for computing the bit position in the associatedPorts array */
#define FM_PACK_ASSOCIATED_PORT(port, type)  ( (type) + (port) * FM_ACL_TYPE_MAX )
#define FM_UNPACK_ASSOCIATED_PORT(value)     ( (value) / FM_ACL_TYPE_MAX )
#define FM_UNPACK_ASSOCIATED_TYPE(value)     ( (value) % FM_ACL_TYPE_MAX )

/*
 * Records the state of the ACLs for this switch as defined by the API. 
 * This is a wrapper to be used in the switch info structure. 
 */ 
typedef struct
{
    /* maps ACL number to fm_acl */
    fm_tree acls;

    /* if global ACL enable is on */
    fm_bool enabled;

    /* maps ACL port set number to fm_aclPortSet */
    fm_tree portSet;

    /* maps mapper key to a mapper entry */
    fm_tree mappers;

    /*************************************************************** 
     * The IDs of the allocated port set.
     * A value of -1 indicate an unused port set.
     * A value of -2 indicates a reserved port set. 
     ***************************************************************/
    fm_int  *portSetId;

} fm_aclInfo;


/*
 * Represents a single policer as defined by the API. 
 *  
 * Allocated by fmCreatePolicer and inserted into the policerInfo.policers 
 * tree for the switch to which the policer belongs. 
 */
typedef struct _fm_individualPolicer
{
    fm_policerConfig attributes;

    /* The bank argument specified to fmCreatePolicer, which for reasons
     * unknown to me is not an attribute like everything else. */
    fm_int           bank;

} fm_individualPolicer;

/*
 * Holds the state of all the policers on a given switch as defined by the 
 * API. 
 */
typedef struct _fm_policerInfo
{
    /* maps policer number to fm_individualPolicer */
    fm_tree policers;

    /*  The switch priority down marking map. The map is an array of integers,
     *  FM_SWITCH_PRIORITY_MAX entries in length, indexed by the current value
     *  of the frame's switch priority with the entry being the new priority
     *  value to mark down to. Corresponds to the FM_POLICER_SWPRI_MKDN_MAP
     *  attribute. See the description of that attribute under
     *  ''Policer Attributes'' for more information.
     *                                                                  \lb\lb
     *  This attribute is global across all policers. */
    fm_int  swPriMkdnMap[FM_SWITCH_PRIORITY_MAX];

    /*  The DSCP down marking map. The map is an array of integers,
     *  FM_MAX_DSCP_VALUES entries in length, indexed by the current value of
     *  the frame's DSCP field with the entry being the new DSCP value to mark
     *  down to. Corresponds to the FM_POLICER_DSCP_MKDN_MAP attribute. See
     *  the description of the FM_POLICER_SWPRI_MKDN_MAP attribute under
     *  ''Policer Attributes'' for more information.
     *                                                                  \lb\lb
     *  This attribute is global across all policers. */
    fm_int  dscpMkdnMap[FM_MAX_DSCP_VALUES];

} fm_policerInfo;

/**************************************************
 * This is a simple structure used to accumulate
 * error messages into a buffer of text.  As messages
 * are added, the statusText pointer is incremented,
 * and the statusTextLength (count of remaining space
 * in the buffer) is decremented.  Error messages
 * increment numErrors, while informational messages
 * do not.
 **************************************************/
typedef struct _fm_aclErrorReporter
{
    fm_text statusText;
    fm_int statusTextLength;
    fm_int numErrors;

} fm_aclErrorReporter;

/* initializes ACLs */
fm_status fmInitACLTable(fm_switch *switchPtr);


/* frees ACLs */
fm_status fmDestroyACLTable(fm_aclInfo *ai);


/* Initializes a static ACL portSet. */
fm_status fmInitStaticACLPortSet(fm_int        sw,
                                 fm_bitArray * bitArray,
                                 fm_int32      portSet);


/* Returns the position of a tuple in the associatedPorts array. */
fm_int fmPackAclAssociatedPort(fm_int sw, fm_int port, fm_aclType type);


/* Converts an associatedPorts bit position to a (port, type) tuple. */
void fmUnpackAclAssociatedPort(fm_int   sw,
                               fm_int   bitNo,
                               fm_aclPortAndType * tuple);


/* Returns a bit in an ACL's associatedPorts array. */
fm_status fmGetAclAssociatedPort(fm_int     sw,
                                 fm_acl   * aclEntry,
                                 fm_int     port,
                                 fm_aclType type,
                                 fm_bool  * bitValue);


/* Sets a bit in an ACL's associatedPorts array. */
fm_status fmSetAclAssociatedPort(fm_int     sw,
                                 fm_acl   * aclEntry,
                                 fm_int     port,
                                 fm_aclType type,
                                 fm_bool    bitValue);


/* initializes policers */
fm_status fmInitPolicers(fm_policerInfo *info);


/* frees policers */
fm_status fmDestroyPolicers(fm_policerInfo *info);

/* Returns TRUE if one or more ACL rules is using a specified
 * virtual network tunnel, FALSE if not. */
fm_status fmIsVNTunnelInUseByACLs(fm_int sw, fm_int tunnelId, fm_bool *inUse);


#endif /* __FM_FM_API_ACL_INT_H */
