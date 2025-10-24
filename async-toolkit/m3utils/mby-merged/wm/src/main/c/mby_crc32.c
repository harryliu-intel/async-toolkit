/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_crc32.c
 * Creation Date:  March 19, 2007
 * Description:    Functions to manipulate bitfields
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2012 Intel Corporation. All Rights Reserved.
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

#include "mby_crc32.h"

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/* CRC-32 least significant bit (LSB) first table.
 * The table has been generated using the following algorithm:
 *
 * fm_uint32 crc;
 * fm_uint32 poly = 0xedb88320U;
 * fm_int    i, j;
 *
 * for (i = 0 ; i < 256 ; i++)
 * {
 *     crc = i;
 *     for (j = 0; j < 8; j++)
 *     {
 *         crc = (crc >> 1) ^ (poly & (-(crc & 1)));
 *     }
 *     fmCrc32Table[i] = crc;
 * }
 */
static const fm_uint32 mbyCrc32Table[] =
{
    0x00000000U, 0x77073096U, 0xee0e612cU, 0x990951baU, 0x076dc419U,
    0x706af48fU, 0xe963a535U, 0x9e6495a3U, 0x0edb8832U, 0x79dcb8a4U,
    0xe0d5e91eU, 0x97d2d988U, 0x09b64c2bU, 0x7eb17cbdU, 0xe7b82d07U,
    0x90bf1d91U, 0x1db71064U, 0x6ab020f2U, 0xf3b97148U, 0x84be41deU,
    0x1adad47dU, 0x6ddde4ebU, 0xf4d4b551U, 0x83d385c7U, 0x136c9856U,
    0x646ba8c0U, 0xfd62f97aU, 0x8a65c9ecU, 0x14015c4fU, 0x63066cd9U,
    0xfa0f3d63U, 0x8d080df5U, 0x3b6e20c8U, 0x4c69105eU, 0xd56041e4U,
    0xa2677172U, 0x3c03e4d1U, 0x4b04d447U, 0xd20d85fdU, 0xa50ab56bU,
    0x35b5a8faU, 0x42b2986cU, 0xdbbbc9d6U, 0xacbcf940U, 0x32d86ce3U,
    0x45df5c75U, 0xdcd60dcfU, 0xabd13d59U, 0x26d930acU, 0x51de003aU,
    0xc8d75180U, 0xbfd06116U, 0x21b4f4b5U, 0x56b3c423U, 0xcfba9599U,
    0xb8bda50fU, 0x2802b89eU, 0x5f058808U, 0xc60cd9b2U, 0xb10be924U,
    0x2f6f7c87U, 0x58684c11U, 0xc1611dabU, 0xb6662d3dU, 0x76dc4190U,
    0x01db7106U, 0x98d220bcU, 0xefd5102aU, 0x71b18589U, 0x06b6b51fU,
    0x9fbfe4a5U, 0xe8b8d433U, 0x7807c9a2U, 0x0f00f934U, 0x9609a88eU,
    0xe10e9818U, 0x7f6a0dbbU, 0x086d3d2dU, 0x91646c97U, 0xe6635c01U,
    0x6b6b51f4U, 0x1c6c6162U, 0x856530d8U, 0xf262004eU, 0x6c0695edU,
    0x1b01a57bU, 0x8208f4c1U, 0xf50fc457U, 0x65b0d9c6U, 0x12b7e950U,
    0x8bbeb8eaU, 0xfcb9887cU, 0x62dd1ddfU, 0x15da2d49U, 0x8cd37cf3U,
    0xfbd44c65U, 0x4db26158U, 0x3ab551ceU, 0xa3bc0074U, 0xd4bb30e2U,
    0x4adfa541U, 0x3dd895d7U, 0xa4d1c46dU, 0xd3d6f4fbU, 0x4369e96aU,
    0x346ed9fcU, 0xad678846U, 0xda60b8d0U, 0x44042d73U, 0x33031de5U,
    0xaa0a4c5fU, 0xdd0d7cc9U, 0x5005713cU, 0x270241aaU, 0xbe0b1010U,
    0xc90c2086U, 0x5768b525U, 0x206f85b3U, 0xb966d409U, 0xce61e49fU,
    0x5edef90eU, 0x29d9c998U, 0xb0d09822U, 0xc7d7a8b4U, 0x59b33d17U,
    0x2eb40d81U, 0xb7bd5c3bU, 0xc0ba6cadU, 0xedb88320U, 0x9abfb3b6U,
    0x03b6e20cU, 0x74b1d29aU, 0xead54739U, 0x9dd277afU, 0x04db2615U,
    0x73dc1683U, 0xe3630b12U, 0x94643b84U, 0x0d6d6a3eU, 0x7a6a5aa8U,
    0xe40ecf0bU, 0x9309ff9dU, 0x0a00ae27U, 0x7d079eb1U, 0xf00f9344U,
    0x8708a3d2U, 0x1e01f268U, 0x6906c2feU, 0xf762575dU, 0x806567cbU,
    0x196c3671U, 0x6e6b06e7U, 0xfed41b76U, 0x89d32be0U, 0x10da7a5aU,
    0x67dd4accU, 0xf9b9df6fU, 0x8ebeeff9U, 0x17b7be43U, 0x60b08ed5U,
    0xd6d6a3e8U, 0xa1d1937eU, 0x38d8c2c4U, 0x4fdff252U, 0xd1bb67f1U,
    0xa6bc5767U, 0x3fb506ddU, 0x48b2364bU, 0xd80d2bdaU, 0xaf0a1b4cU,
    0x36034af6U, 0x41047a60U, 0xdf60efc3U, 0xa867df55U, 0x316e8eefU,
    0x4669be79U, 0xcb61b38cU, 0xbc66831aU, 0x256fd2a0U, 0x5268e236U,
    0xcc0c7795U, 0xbb0b4703U, 0x220216b9U, 0x5505262fU, 0xc5ba3bbeU,
    0xb2bd0b28U, 0x2bb45a92U, 0x5cb36a04U, 0xc2d7ffa7U, 0xb5d0cf31U,
    0x2cd99e8bU, 0x5bdeae1dU, 0x9b64c2b0U, 0xec63f226U, 0x756aa39cU,
    0x026d930aU, 0x9c0906a9U, 0xeb0e363fU, 0x72076785U, 0x05005713U,
    0x95bf4a82U, 0xe2b87a14U, 0x7bb12baeU, 0x0cb61b38U, 0x92d28e9bU,
    0xe5d5be0dU, 0x7cdcefb7U, 0x0bdbdf21U, 0x86d3d2d4U, 0xf1d4e242U,
    0x68ddb3f8U, 0x1fda836eU, 0x81be16cdU, 0xf6b9265bU, 0x6fb077e1U,
    0x18b74777U, 0x88085ae6U, 0xff0f6a70U, 0x66063bcaU, 0x11010b5cU,
    0x8f659effU, 0xf862ae69U, 0x616bffd3U, 0x166ccf45U, 0xa00ae278U,
    0xd70dd2eeU, 0x4e048354U, 0x3903b3c2U, 0xa7672661U, 0xd06016f7U,
    0x4969474dU, 0x3e6e77dbU, 0xaed16a4aU, 0xd9d65adcU, 0x40df0b66U,
    0x37d83bf0U, 0xa9bcae53U, 0xdebb9ec5U, 0x47b2cf7fU, 0x30b5ffe9U,
    0xbdbdf21cU, 0xcabac28aU, 0x53b39330U, 0x24b4a3a6U, 0xbad03605U,
    0xcdd70693U, 0x54de5729U, 0x23d967bfU, 0xb3667a2eU, 0xc4614ab8U,
    0x5d681b02U, 0x2a6f2b94U, 0xb40bbe37U, 0xc30c8ea1U, 0x5a05df1bU,
    0x2d02ef8dU
};

/* CRC-32C least significant bit (LSB) first table.
 * The table has been generated using the following algorithm:
 *
 * fm_uint32 crc;
 * fm_uint32 poly = 0x82f63b78U;
 * fm_int    i, j;
 *
 * for (i = 0 ; i < 256 ; i++)
 * {
 *     crc = i;
 *     for (j = 0; j < 8; j++)
 *     {
 *         crc = (crc >> 1) ^ (poly & (-(crc & 1)));
 *     }
 *     mbyCrc32CTable[i] = crc;
 * }
 */
static const fm_uint32 mbyCrc32CTable[] =
{
    0x00000000U, 0xf26b8303U, 0xe13b70f7U, 0x1350f3f4U, 0xc79a971fU,
    0x35f1141cU, 0x26a1e7e8U, 0xd4ca64ebU, 0x8ad958cfU, 0x78b2dbccU,
    0x6be22838U, 0x9989ab3bU, 0x4d43cfd0U, 0xbf284cd3U, 0xac78bf27U,
    0x5e133c24U, 0x105ec76fU, 0xe235446cU, 0xf165b798U, 0x030e349bU,
    0xd7c45070U, 0x25afd373U, 0x36ff2087U, 0xc494a384U, 0x9a879fa0U,
    0x68ec1ca3U, 0x7bbcef57U, 0x89d76c54U, 0x5d1d08bfU, 0xaf768bbcU,
    0xbc267848U, 0x4e4dfb4bU, 0x20bd8edeU, 0xd2d60dddU, 0xc186fe29U,
    0x33ed7d2aU, 0xe72719c1U, 0x154c9ac2U, 0x061c6936U, 0xf477ea35U,
    0xaa64d611U, 0x580f5512U, 0x4b5fa6e6U, 0xb93425e5U, 0x6dfe410eU,
    0x9f95c20dU, 0x8cc531f9U, 0x7eaeb2faU, 0x30e349b1U, 0xc288cab2U,
    0xd1d83946U, 0x23b3ba45U, 0xf779deaeU, 0x05125dadU, 0x1642ae59U,
    0xe4292d5aU, 0xba3a117eU, 0x4851927dU, 0x5b016189U, 0xa96ae28aU,
    0x7da08661U, 0x8fcb0562U, 0x9c9bf696U, 0x6ef07595U, 0x417b1dbcU,
    0xb3109ebfU, 0xa0406d4bU, 0x522bee48U, 0x86e18aa3U, 0x748a09a0U,
    0x67dafa54U, 0x95b17957U, 0xcba24573U, 0x39c9c670U, 0x2a993584U,
    0xd8f2b687U, 0x0c38d26cU, 0xfe53516fU, 0xed03a29bU, 0x1f682198U,
    0x5125dad3U, 0xa34e59d0U, 0xb01eaa24U, 0x42752927U, 0x96bf4dccU,
    0x64d4cecfU, 0x77843d3bU, 0x85efbe38U, 0xdbfc821cU, 0x2997011fU,
    0x3ac7f2ebU, 0xc8ac71e8U, 0x1c661503U, 0xee0d9600U, 0xfd5d65f4U,
    0x0f36e6f7U, 0x61c69362U, 0x93ad1061U, 0x80fde395U, 0x72966096U,
    0xa65c047dU, 0x5437877eU, 0x4767748aU, 0xb50cf789U, 0xeb1fcbadU,
    0x197448aeU, 0x0a24bb5aU, 0xf84f3859U, 0x2c855cb2U, 0xdeeedfb1U,
    0xcdbe2c45U, 0x3fd5af46U, 0x7198540dU, 0x83f3d70eU, 0x90a324faU,
    0x62c8a7f9U, 0xb602c312U, 0x44694011U, 0x5739b3e5U, 0xa55230e6U,
    0xfb410cc2U, 0x092a8fc1U, 0x1a7a7c35U, 0xe811ff36U, 0x3cdb9bddU,
    0xceb018deU, 0xdde0eb2aU, 0x2f8b6829U, 0x82f63b78U, 0x709db87bU,
    0x63cd4b8fU, 0x91a6c88cU, 0x456cac67U, 0xb7072f64U, 0xa457dc90U,
    0x563c5f93U, 0x082f63b7U, 0xfa44e0b4U, 0xe9141340U, 0x1b7f9043U,
    0xcfb5f4a8U, 0x3dde77abU, 0x2e8e845fU, 0xdce5075cU, 0x92a8fc17U,
    0x60c37f14U, 0x73938ce0U, 0x81f80fe3U, 0x55326b08U, 0xa759e80bU,
    0xb4091bffU, 0x466298fcU, 0x1871a4d8U, 0xea1a27dbU, 0xf94ad42fU,
    0x0b21572cU, 0xdfeb33c7U, 0x2d80b0c4U, 0x3ed04330U, 0xccbbc033U,
    0xa24bb5a6U, 0x502036a5U, 0x4370c551U, 0xb11b4652U, 0x65d122b9U,
    0x97baa1baU, 0x84ea524eU, 0x7681d14dU, 0x2892ed69U, 0xdaf96e6aU,
    0xc9a99d9eU, 0x3bc21e9dU, 0xef087a76U, 0x1d63f975U, 0x0e330a81U,
    0xfc588982U, 0xb21572c9U, 0x407ef1caU, 0x532e023eU, 0xa145813dU,
    0x758fe5d6U, 0x87e466d5U, 0x94b49521U, 0x66df1622U, 0x38cc2a06U,
    0xcaa7a905U, 0xd9f75af1U, 0x2b9cd9f2U, 0xff56bd19U, 0x0d3d3e1aU,
    0x1e6dcdeeU, 0xec064eedU, 0xc38d26c4U, 0x31e6a5c7U, 0x22b65633U,
    0xd0ddd530U, 0x0417b1dbU, 0xf67c32d8U, 0xe52cc12cU, 0x1747422fU,
    0x49547e0bU, 0xbb3ffd08U, 0xa86f0efcU, 0x5a048dffU, 0x8ecee914U,
    0x7ca56a17U, 0x6ff599e3U, 0x9d9e1ae0U, 0xd3d3e1abU, 0x21b862a8U,
    0x32e8915cU, 0xc083125fU, 0x144976b4U, 0xe622f5b7U, 0xf5720643U,
    0x07198540U, 0x590ab964U, 0xab613a67U, 0xb831c993U, 0x4a5a4a90U,
    0x9e902e7bU, 0x6cfbad78U, 0x7fab5e8cU, 0x8dc0dd8fU, 0xe330a81aU,
    0x115b2b19U, 0x020bd8edU, 0xf0605beeU, 0x24aa3f05U, 0xd6c1bc06U,
    0xc5914ff2U, 0x37faccf1U, 0x69e9f0d5U, 0x9b8273d6U, 0x88d28022U,
    0x7ab90321U, 0xae7367caU, 0x5c18e4c9U, 0x4f48173dU, 0xbd23943eU,
    0xf36e6f75U, 0x0105ec76U, 0x12551f82U, 0xe03e9c81U, 0x34f4f86aU,
    0xc69f7b69U, 0xd5cf889dU, 0x27a40b9eU, 0x79b737baU, 0x8bdcb4b9U,
    0x988c474dU, 0x6ae7c44eU, 0xbe2da0a5U, 0x4c4623a6U, 0x5f16d052U,
    0xad7d5351U
};

/* CRC-32K least significant bit (LSB) first table.
 * The table has been generated using the following algorithm:
 *
 * fm_uint32 crc;
 * fm_uint32 poly = 0xEB31D82EU;
 * fm_int    i, j;
 *
 * for (i = 0 ; i < 256 ; i++)
 * {
 *     crc = i;
 *     for (j = 0; j < 8; j++)
 *     {
 *         crc = (crc >> 1) ^ (poly & (-(crc & 1)));
 *     }
 *     mbyCrc32KTable[i] = crc;
 * }
 */
static const fm_uint32 mbyCrc32KTable[] =
{
    0x00000000U, 0x9695c4caU, 0xfb4839c9U, 0x6dddfd03U, 0x20f3c3cfU,
    0xb6660705U, 0xdbbbfa06U, 0x4d2e3eccU, 0x41e7879eU, 0xd7724354U,
    0xbaafbe57U, 0x2c3a7a9dU, 0x61144451U, 0xf781809bU, 0x9a5c7d98U,
    0x0cc9b952U, 0x83cf0f3cU, 0x155acbf6U, 0x788736f5U, 0xee12f23fU,
    0xa33cccf3U, 0x35a90839U, 0x5874f53aU, 0xcee131f0U, 0xc22888a2U,
    0x54bd4c68U, 0x3960b16bU, 0xaff575a1U, 0xe2db4b6dU, 0x744e8fa7U,
    0x199372a4U, 0x8f06b66eU, 0xd1fdae25U, 0x47686aefU, 0x2ab597ecU,
    0xbc205326U, 0xf10e6deaU, 0x679ba920U, 0x0a465423U, 0x9cd390e9U,
    0x901a29bbU, 0x068fed71U, 0x6b521072U, 0xfdc7d4b8U, 0xb0e9ea74U,
    0x267c2ebeU, 0x4ba1d3bdU, 0xdd341777U, 0x5232a119U, 0xc4a765d3U,
    0xa97a98d0U, 0x3fef5c1aU, 0x72c162d6U, 0xe454a61cU, 0x89895b1fU,
    0x1f1c9fd5U, 0x13d52687U, 0x8540e24dU, 0xe89d1f4eU, 0x7e08db84U,
    0x3326e548U, 0xa5b32182U, 0xc86edc81U, 0x5efb184bU, 0x7598ec17U,
    0xe30d28ddU, 0x8ed0d5deU, 0x18451114U, 0x556b2fd8U, 0xc3feeb12U,
    0xae231611U, 0x38b6d2dbU, 0x347f6b89U, 0xa2eaaf43U, 0xcf375240U,
    0x59a2968aU, 0x148ca846U, 0x82196c8cU, 0xefc4918fU, 0x79515545U,
    0xf657e32bU, 0x60c227e1U, 0x0d1fdae2U, 0x9b8a1e28U, 0xd6a420e4U,
    0x4031e42eU, 0x2dec192dU, 0xbb79dde7U, 0xb7b064b5U, 0x2125a07fU,
    0x4cf85d7cU, 0xda6d99b6U, 0x9743a77aU, 0x01d663b0U, 0x6c0b9eb3U,
    0xfa9e5a79U, 0xa4654232U, 0x32f086f8U, 0x5f2d7bfbU, 0xc9b8bf31U,
    0x849681fdU, 0x12034537U, 0x7fdeb834U, 0xe94b7cfeU, 0xe582c5acU,
    0x73170166U, 0x1ecafc65U, 0x885f38afU, 0xc5710663U, 0x53e4c2a9U,
    0x3e393faaU, 0xa8acfb60U, 0x27aa4d0eU, 0xb13f89c4U, 0xdce274c7U,
    0x4a77b00dU, 0x07598ec1U, 0x91cc4a0bU, 0xfc11b708U, 0x6a8473c2U,
    0x664dca90U, 0xf0d80e5aU, 0x9d05f359U, 0x0b903793U, 0x46be095fU,
    0xd02bcd95U, 0xbdf63096U, 0x2b63f45cU, 0xeb31d82eU, 0x7da41ce4U,
    0x1079e1e7U, 0x86ec252dU, 0xcbc21be1U, 0x5d57df2bU, 0x308a2228U,
    0xa61fe6e2U, 0xaad65fb0U, 0x3c439b7aU, 0x519e6679U, 0xc70ba2b3U,
    0x8a259c7fU, 0x1cb058b5U, 0x716da5b6U, 0xe7f8617cU, 0x68fed712U,
    0xfe6b13d8U, 0x93b6eedbU, 0x05232a11U, 0x480d14ddU, 0xde98d017U,
    0xb3452d14U, 0x25d0e9deU, 0x2919508cU, 0xbf8c9446U, 0xd2516945U,
    0x44c4ad8fU, 0x09ea9343U, 0x9f7f5789U, 0xf2a2aa8aU, 0x64376e40U,
    0x3acc760bU, 0xac59b2c1U, 0xc1844fc2U, 0x57118b08U, 0x1a3fb5c4U,
    0x8caa710eU, 0xe1778c0dU, 0x77e248c7U, 0x7b2bf195U, 0xedbe355fU,
    0x8063c85cU, 0x16f60c96U, 0x5bd8325aU, 0xcd4df690U, 0xa0900b93U,
    0x3605cf59U, 0xb9037937U, 0x2f96bdfdU, 0x424b40feU, 0xd4de8434U,
    0x99f0baf8U, 0x0f657e32U, 0x62b88331U, 0xf42d47fbU, 0xf8e4fea9U,
    0x6e713a63U, 0x03acc760U, 0x953903aaU, 0xd8173d66U, 0x4e82f9acU,
    0x235f04afU, 0xb5cac065U, 0x9ea93439U, 0x083cf0f3U, 0x65e10df0U,
    0xf374c93aU, 0xbe5af7f6U, 0x28cf333cU, 0x4512ce3fU, 0xd3870af5U,
    0xdf4eb3a7U, 0x49db776dU, 0x24068a6eU, 0xb2934ea4U, 0xffbd7068U,
    0x6928b4a2U, 0x04f549a1U, 0x92608d6bU, 0x1d663b05U, 0x8bf3ffcfU,
    0xe62e02ccU, 0x70bbc606U, 0x3d95f8caU, 0xab003c00U, 0xc6ddc103U,
    0x504805c9U, 0x5c81bc9bU, 0xca147851U, 0xa7c98552U, 0x315c4198U,
    0x7c727f54U, 0xeae7bb9eU, 0x873a469dU, 0x11af8257U, 0x4f549a1cU,
    0xd9c15ed6U, 0xb41ca3d5U, 0x2289671fU, 0x6fa759d3U, 0xf9329d19U,
    0x94ef601aU, 0x027aa4d0U, 0x0eb31d82U, 0x9826d948U, 0xf5fb244bU,
    0x636ee081U, 0x2e40de4dU, 0xb8d51a87U, 0xd508e784U, 0x439d234eU,
    0xcc9b9520U, 0x5a0e51eaU, 0x37d3ace9U, 0xa1466823U, 0xec6856efU,
    0x7afd9225U, 0x17206f26U, 0x81b5abecU, 0x8d7c12beU, 0x1be9d674U,
    0x76342b77U, 0xe0a1efbdU, 0xad8fd171U, 0x3b1a15bbU, 0x56c7e8b8U,
    0xc0522c72U
};

/* CRC-32Q least significant bit (LSB) first table.
 * The table has been generated using the following algorithm:
 *
 * fm_uint32 crc;
 * fm_uint32 poly = 0xD5828281U;
 * fm_int    i, j;
 *
 * for (i = 0 ; i < 256 ; i++)
 * {
 *     crc = i;
 *     for (j = 0; j < 8; j++)
 *     {
 *         crc = (crc >> 1) ^ (poly & (-(crc & 1)));
 *     }
 *     mbyCrc32QTable[i] = crc;
 * }
 */
static const fm_uint32 mbyCrc32QTable[] =
{
    0x00000000U, 0x999a0002U, 0x98310507U, 0x01ab0505U, 0x9b670f0dU,
    0x02fd0f0fU, 0x03560a0aU, 0x9acc0a08U, 0x9dcb1b19U, 0x04511b1bU,
    0x05fa1e1eU, 0x9c601e1cU, 0x06ac1414U, 0x9f361416U, 0x9e9d1113U,
    0x07071111U, 0x90933331U, 0x09093333U, 0x08a23636U, 0x91383634U,
    0x0bf43c3cU, 0x926e3c3eU, 0x93c5393bU, 0x0a5f3939U, 0x0d582828U,
    0x94c2282aU, 0x95692d2fU, 0x0cf32d2dU, 0x963f2725U, 0x0fa52727U,
    0x0e0e2222U, 0x97942220U, 0x8a236361U, 0x13b96363U, 0x12126666U,
    0x8b886664U, 0x11446c6cU, 0x88de6c6eU, 0x8975696bU, 0x10ef6969U,
    0x17e87878U, 0x8e72787aU, 0x8fd97d7fU, 0x16437d7dU, 0x8c8f7775U,
    0x15157777U, 0x14be7272U, 0x8d247270U, 0x1ab05050U, 0x832a5052U,
    0x82815557U, 0x1b1b5555U, 0x81d75f5dU, 0x184d5f5fU, 0x19e65a5aU,
    0x807c5a58U, 0x877b4b49U, 0x1ee14b4bU, 0x1f4a4e4eU, 0x86d04e4cU,
    0x1c1c4444U, 0x85864446U, 0x842d4143U, 0x1db74141U, 0xbf43c3c1U,
    0x26d9c3c3U, 0x2772c6c6U, 0xbee8c6c4U, 0x2424ccccU, 0xbdbeccceU,
    0xbc15c9cbU, 0x258fc9c9U, 0x2288d8d8U, 0xbb12d8daU, 0xbab9dddfU,
    0x2323ddddU, 0xb9efd7d5U, 0x2075d7d7U, 0x21ded2d2U, 0xb844d2d0U,
    0x2fd0f0f0U, 0xb64af0f2U, 0xb7e1f5f7U, 0x2e7bf5f5U, 0xb4b7fffdU,
    0x2d2dffffU, 0x2c86fafaU, 0xb51cfaf8U, 0xb21bebe9U, 0x2b81ebebU,
    0x2a2aeeeeU, 0xb3b0eeecU, 0x297ce4e4U, 0xb0e6e4e6U, 0xb14de1e3U,
    0x28d7e1e1U, 0x3560a0a0U, 0xacfaa0a2U, 0xad51a5a7U, 0x34cba5a5U,
    0xae07afadU, 0x379dafafU, 0x3636aaaaU, 0xafacaaa8U, 0xa8abbbb9U,
    0x3131bbbbU, 0x309abebeU, 0xa900bebcU, 0x33ccb4b4U, 0xaa56b4b6U,
    0xabfdb1b3U, 0x3267b1b1U, 0xa5f39391U, 0x3c699393U, 0x3dc29696U,
    0xa4589694U, 0x3e949c9cU, 0xa70e9c9eU, 0xa6a5999bU, 0x3f3f9999U,
    0x38388888U, 0xa1a2888aU, 0xa0098d8fU, 0x39938d8dU, 0xa35f8785U,
    0x3ac58787U, 0x3b6e8282U, 0xa2f48280U, 0xd5828281U, 0x4c188283U,
    0x4db38786U, 0xd4298784U, 0x4ee58d8cU, 0xd77f8d8eU, 0xd6d4888bU,
    0x4f4e8889U, 0x48499998U, 0xd1d3999aU, 0xd0789c9fU, 0x49e29c9dU,
    0xd32e9695U, 0x4ab49697U, 0x4b1f9392U, 0xd2859390U, 0x4511b1b0U,
    0xdc8bb1b2U, 0xdd20b4b7U, 0x44bab4b5U, 0xde76bebdU, 0x47ecbebfU,
    0x4647bbbaU, 0xdfddbbb8U, 0xd8daaaa9U, 0x4140aaabU, 0x40ebafaeU,
    0xd971afacU, 0x43bda5a4U, 0xda27a5a6U, 0xdb8ca0a3U, 0x4216a0a1U,
    0x5fa1e1e0U, 0xc63be1e2U, 0xc790e4e7U, 0x5e0ae4e5U, 0xc4c6eeedU,
    0x5d5ceeefU, 0x5cf7ebeaU, 0xc56debe8U, 0xc26afaf9U, 0x5bf0fafbU,
    0x5a5bfffeU, 0xc3c1fffcU, 0x590df5f4U, 0xc097f5f6U, 0xc13cf0f3U,
    0x58a6f0f1U, 0xcf32d2d1U, 0x56a8d2d3U, 0x5703d7d6U, 0xce99d7d4U,
    0x5455dddcU, 0xcdcfdddeU, 0xcc64d8dbU, 0x55fed8d9U, 0x52f9c9c8U,
    0xcb63c9caU, 0xcac8cccfU, 0x5352cccdU, 0xc99ec6c5U, 0x5004c6c7U,
    0x51afc3c2U, 0xc835c3c0U, 0x6ac14140U, 0xf35b4142U, 0xf2f04447U,
    0x6b6a4445U, 0xf1a64e4dU, 0x683c4e4fU, 0x69974b4aU, 0xf00d4b48U,
    0xf70a5a59U, 0x6e905a5bU, 0x6f3b5f5eU, 0xf6a15f5cU, 0x6c6d5554U,
    0xf5f75556U, 0xf45c5053U, 0x6dc65051U, 0xfa527271U, 0x63c87273U,
    0x62637776U, 0xfbf97774U, 0x61357d7cU, 0xf8af7d7eU, 0xf904787bU,
    0x609e7879U, 0x67996968U, 0xfe03696aU, 0xffa86c6fU, 0x66326c6dU,
    0xfcfe6665U, 0x65646667U, 0x64cf6362U, 0xfd556360U, 0xe0e22221U,
    0x79782223U, 0x78d32726U, 0xe1492724U, 0x7b852d2cU, 0xe21f2d2eU,
    0xe3b4282bU, 0x7a2e2829U, 0x7d293938U, 0xe4b3393aU, 0xe5183c3fU,
    0x7c823c3dU, 0xe64e3635U, 0x7fd43637U, 0x7e7f3332U, 0xe7e53330U,
    0x70711110U, 0xe9eb1112U, 0xe8401417U, 0x71da1415U, 0xeb161e1dU,
    0x728c1e1fU, 0x73271b1aU, 0xeabd1b18U, 0xedba0a09U, 0x74200a0bU,
    0x758b0f0eU, 0xec110f0cU, 0x76dd0504U, 0xef470506U, 0xeeec0003U,
    0x77760001U
};

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** mbyCrc32
 * \ingroup intUtil
 *
 * \desc            Returns the Ethernet 32-bit cyclic redundancy check
 *                  (CRC-32) of the given buffer.
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32.
 *
 * \return          The CRC-32 value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = 0xffffffffU;

    for (fm_uint i = 0 ; i < len ; i++)
        crc = mbyCrc32Table[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);

    crc = ~crc;

    return crc;

}   /* end mbyCrc32 */


/*****************************************************************************/
/** mbyCrc32Math
 * \ingroup intUtil
 *
 * \desc            Returns the Ethernet 32-bit cyclic redundancy check
 *                  (CRC-32) of the given buffer with the CRC initialized
 *                  to zero.
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32.
 *
 * \return          The CRC-32 value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32Math(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = 0;

    for (fm_uint i = 0 ; i < len ; i++)
        crc = mbyCrc32Table[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);

    return crc;

}   /* end mbyCrc32Math */

/*****************************************************************************/
/** mbyCrc32ByteSwap
 * \ingroup intUtil
 *
 * \desc            Returns the Ethernet 32-bit cyclic redundancy check
 *                  (CRC-32) of the given buffer with the CRC initialized
 *                  to zero. CRC is byte swapped
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32.
 *
 * \return          The CRC-32 value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32ByteSwap(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = mbyCrc32Math(buf, len);
    
    //Byte swap
    crc = ((crc >> 24) & 0xff) |
          ((crc << 8) & 0xff0000) |
          ((crc >> 8) & 0xff00) |
          ((crc << 24) & 0xff000000);

    return crc;

} /* end mbyCrc32ByteSwap */

/*****************************************************************************/
/** mbyCrc32C
 * \ingroup intUtil
 *
 * \desc            Returns the Castagnoli 32-bit cyclic redundancy check
 *                  (CRC-32C) of the given buffer.
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32C.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32C.
 *
 * \return          The CRC-32C value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32C(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = 0xffffffffU;

    for (fm_uint i = 0 ; i < len ; i++)
        crc = mbyCrc32CTable[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);

    crc = ~crc;

    return crc;

}   /* end mbyCrc32C */

/*****************************************************************************/
/** mbyCrc32CMath
 * \ingroup intUtil
 *
 * \desc            Returns the Ethernet 32-bit cyclic redundancy check
 *                  (CRC-32) of the given buffer with the CRC initialized
 *                  to zero.
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32.
 *
 * \return          The CRC-32 value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32CMath(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = 0;

    for (fm_uint i = 0 ; i < len ; i++)
        crc = mbyCrc32CTable[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);

    return crc;

} /* end mbyCrc32Math */

/*****************************************************************************/
/** mbyCrc32CByteSwap
 * \ingroup intUtil
 *
 * \desc            Returns the Ethernet 32-bit cyclic redundancy check
 *                  (CRC-32) of the given buffer with the CRC initialized
 *                  to zero. CRC is byteswapped
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32.
 *
 * \return          The CRC-32 value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32CByteSwap(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = mbyCrc32CMath(buf, len);
    
    // Byte swap
    crc = ((crc >> 24) & 0xff) |
          ((crc <<  8) & 0xff0000) |
          ((crc >>  8) & 0xff00) |
          ((crc << 24) & 0xff000000);

    return crc;

} /* end mbyCrc32CByteSwap */

/*****************************************************************************/
/** mbyCrc32K
 * \ingroup intUtil
 *
 * \desc            Returns the Koopman 32-bit cyclic redundancy check
 *                  (CRC-32K) of the given buffer.
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32K.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32K.
 *
 * \return          The CRC-32K value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32K(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = 0xffffffffU;

    for (fm_uint i = 0 ; i < len ; i++)
        crc = mbyCrc32KTable[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);

    crc = ~crc;

    return crc;

} /* end mbyCrc32K */

/*****************************************************************************/
/** mbyCrc32KMath
 * \ingroup intUtil
 *
 * \desc            Returns the Koopman 32-bit cyclic redundancy check
 *                  (CRC-32K) of the given buffer with the CRC initialized
 *                  to zero.
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32K.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32K.
 *
 * \return          The CRC-32K value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32KMath(const fm_byte *const buf, const fm_uint32 len)
{
    fm_uint32 crc = 0;

    for (fm_uint i = 0 ; i < len ; i++)
        crc = mbyCrc32KTable[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);

    return crc;

}   /* end mbyCrc32KMath */

/*****************************************************************************/
/** mbyCrc32CByteSwap
 * \ingroup intUtil
 *
 * \desc            Returns theKoopman 32-bit cyclic redundancy check
 *                  (CRC-32K) of the given buffer with the CRC initialized
 *                  to zero. CRC is byteswapped
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32K.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32K.
 *
 * \return          The CRC-32K value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32KByteSwap(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = mbyCrc32KMath(buf, len);
    
    //Byte swap
    crc = ((crc >> 24) & 0xff) |
          ((crc <<  8) & 0xff0000) |
          ((crc >>  8) & 0xff00) |
          ((crc << 24) & 0xff000000);

    return crc;

} /* end mbyCrc32KByteSwap */

/*****************************************************************************/
/** mbyCrc32Q
 * \ingroup intUtil
 *
 * \desc            Returns the AIXM 32-bit cyclic redundancy check
 *                  (CRC-32Q) of the given buffer.
 *
 * \param[in]       buf points to the buffer for which to compute the CRC-32Q.
 *
 * \param[in]       len is the number of bytes to include in the CRC-32Q.
 *
 * \return          The CRC-32Q value.
 *
 *****************************************************************************/
fm_uint32 mbyCrc32Q(const fm_byte * const buf, const fm_uint32 len)
{
    fm_uint32 crc = 0xffffffffU;

    for (fm_uint i = 0 ; i < len ; i++)
        crc = mbyCrc32QTable[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);

    crc = ~crc;

    return crc;

}   /* end mbyCrc32Q */
