/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_attr.h
 * Creation Date:   June 5, 2012
 * Description:     Platform specific attributes
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_PLATFORM_ATTR_H
#define __FM_PLATFORM_ATTR_H

/****************************************************************************/
/* \ingroup constPlatformAttr
 *
 * Platform attributes, used as an argument to ''fmPlatformSetAttribute'' and
 * ''fmPlatformGetAttribute''. This list of attribute values are provided as
 * an example, being the attributes of the FM85XXEP Fulcrum reference design.
 * Other platforms may or may not support these same attributes, and may
 * support other attributes as well. These attributes and the
 * ''fmPlatformSetAttribute'' and ''fmPlatformGetAttribute'' services are
 * provided by the platform layer for the benefit of the application; they are
 * not directly used by the core API.
 *                                                                      \lb\lb
 * For each attribute, the data type of the corresponding attribute value is
 * indicated.
 *                                                                      \lb\lb
 * Unless otherwise indicated, the sw and index arguments to
 * ''fmPlatformSetAttribute'' and ''fmPlatformGetAttribute'' are not used.
 ****************************************************************************/
enum _fm_platformAttr
{
    /** Type fm_int: The maximum number of bytes send by the socket thread 
     *  to other sockets before switching back to its server socket. The
     *  server stops after this limit is reached, a value of 0 will send
     *  one message max. */
    FM_PLATFORM_ATTR_EGRESS_LIMIT = 0,

    /** Type fm_text: Indicate the name of the topology file which lists the
     *  internal link connections between white model instances. This file
     *  must be located in the path specified by the WMODEL_INFO_PATH 
     *  environment variable. If multiple units (API instances) are included
     *  in a single topology, each unit must use the same path. 
     *                                                                  \lb\lb
     *  The topology file is an ASCII text file, one line per internal link,
     *  each link specified using the following format:
     *                                                                  \lb\lb
     *  <Unit1>,<Switch1>,<Port1>,<Unit2>,<Switch2>,<Port2>
     *                                                                  \lb\lb
     *  For example:
     *                                                                  \lb\lb
     *  0,0,1,0,1,10                                                    \lb
     *  0,0,2,1,2,20
     *                                                                  \lb\lb
     *  specifies that:
     *                                                                  \lb\lb
     *  Unit 0, Switch 0, Port 1 is connected to Unit 0, Switch 1, Port 10
     *                                                                  \lb\lb
     *  Unit 0, Switch 0, Port 2 is connected to Unit 1, Switch 2, Port 20
     *                                                                  \lb\lb
     *  Note that each Unit (API instance) can manage one or more switches.
     *  However, ''FM_MAX_NUM_FOCALPOINTS'' must be defined as the maximum
     *  number of switches per unit and all API instances must be initialized
     *  prior to setting this attribute. The API attribute 
     *  ''api.platform.model.position'' is used to identify the unit number
     *  for each API instance. 
     *                                                                  \lb\lb
     *  See ''Multiple Model Environments'' in the FocalPoint API User Guide 
     *  for more information. */
    FM_PLATFORM_ATTR_TOPOLOGY,

    /** Not used in this platform, but defined so tests will compile. */
    FM_PLATFORM_ATTR_DMA,

    /** For internal use only. */
    FM_PLATFORM_ATTR_MAX

};




#endif /* __FM_PLATFORM_ATTR_H */
