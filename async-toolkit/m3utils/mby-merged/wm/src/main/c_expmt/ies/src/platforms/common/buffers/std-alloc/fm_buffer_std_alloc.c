/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_buffer_std_alloc.c
 * Creation Date:   September 19, 2007
 * Description:     Common module to manage the free buffers as a circular
 *                  list.  Relies on the following to be used:
 *
 *                  - the state structure fm_bufferAllocState must be defined
 *                    in the platform root as the field bufferAllocState
 *                  - the platform must define fmPlatformGetBufferCount to
 *                    return the number of buffers to allocate
 *                  - the platform must define the following constants:
 *                      FM_BUFFER_SIZE_WORDS
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


#include <fm_sdk_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** GetBufferMemory
 * \ingroup intPlatform
 *
 * \desc            Returns the memory for a given chunk.  Assumes that the
 *                  pool field has already been setup.
 *
 * \param[in]       index is the chunk number from 0..N-1, where N is the value
 *                  of FM_NUM_BUFFERS
 *
 * \return          the pointer to the chunk memory.
 *
 *****************************************************************************/
static fm_uint32 *GetBufferMemory(fm_int index)
{
    return fmRootPlatform->bufferAllocState.pool + (index * FM_BUFFER_SIZE_WORDS);

}   /* end GetBufferMemory */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmPlatformInitBuffers
 * \ingroup intPlatform
 *
 * \desc            Initializes the buffer allocator subsystem
 *
 * \param[in]       bufferMemoryPool points to the caller allocated memory used
 *                  to back the buffer data.
 *
 * \return          fm_status code
 *
 *****************************************************************************/
fm_status fmPlatformInitBuffers(fm_uint32 *bufferMemoryPool)
{
    int                  i;
    fm_bufferAllocState *info;

    FM_LOG_ENTRY(FM_LOG_CAT_BUFFER, "pool=%p\n", (void *) bufferMemoryPool);
    
    info = &fmRootPlatform->bufferAllocState;

    memset( info, 0, sizeof(fm_bufferAllocState) );

    info->pool             = bufferMemoryPool;
    info->totalBufferCount = FM_NUM_BUFFERS;
    info->availableBuffers = info->totalBufferCount;

    info->table = (fm_buffer *) fmAlloc(sizeof(fm_buffer) *
                                        info->totalBufferCount);

    if (info->table == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_BUFFER, FM_ERR_NO_MEM);
    }

    /**************************************************
     * Initialize allocator.  Each buffer in the pool
     * is represented by its index into the pool memory
     * block.  The allocator is an array of buffer
     * indices, indexed by a buffer's index.  The cell
     * content is the index of the next buffer in a
     * virtual list of chunks.
     *
     * The list is terminated with a -1 entry.
     *
     * The for-loop below initializes the array as:
     *
     *          0    1    2    3         fd   fe   ff
     *       +----+----+----+----+-----+----+----+----+
     *       |  1 |  2 |  3 |  4 | ... | fe | ff | -1 |
     *       +----+----+----+----+-----+----+----+----+
     *
     * for the case where the pool contains 256 buffers.
     **************************************************/

    /* Allocate the allocator. */
    info->freeList = (fm_int *) fmAlloc(sizeof(fm_int) *
                                        info->totalBufferCount);

    if (info->freeList == NULL)
    {
        fmFree(info->table);
        info->table = NULL;
        FM_LOG_EXIT(FM_LOG_CAT_BUFFER, FM_ERR_NO_MEM);
    }

    /* Index of first buffer in list - used for allocating. */
    info->firstFree = 0;

    /* Initialize the allocator array. */
    for (i = 0 ; i < info->totalBufferCount ; i++)
    {
        info->freeList[i] = i + 1;
    }

    /* Terminate the list. */
    info->freeList[info->totalBufferCount - 1] = -1;

    /* Initialize all buffers */
    for (i = 0 ; i < info->totalBufferCount ; i++)
    {
        info->table[i].data = GetBufferMemory(i);

        info->table[i].next  = NULL;
        info->table[i].len   = 0;
        info->table[i].index = i;

        /**
         * Also initialize all the data areas.  This causes the page
         * faults needed to map the data areas into user memory to all
         * occur now, saving time during frame transmission/reception
         * later.
         */
        memset(info->table[i].data, 'z', FM_BUFFER_SIZE_BYTES);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_BUFFER,
                 "Initialized buffers, %d left\n",
                 info->availableBuffers);

    FM_LOG_EXIT(FM_LOG_CAT_BUFFER, FM_OK);

}   /* end fmPlatformInitBuffers */




/*****************************************************************************/
/** fmPlatformAllocateBuffer
 * \ingroup platform
 *
 * \desc            Allocate a packet buffer.  The buffer is accessed through
 *                  an ''fm_buffer'' structure, which keeps housekeeping
 *                  information for chaining multiple data blocks together.
 *
 * \return          A pointer to an ''fm_buffer'' structure.
 * \return          NULL is returned if there are no buffers available.
 *
 *****************************************************************************/
fm_buffer *fmPlatformAllocateBuffer(void)
{
    fm_bufferAllocState *info;
    fm_int               chunk;
    fm_buffer *          ret;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_BUFFER);
    
    info = &fmRootPlatform->bufferAllocState;

    TAKE_PLAT_STATE_LOCK();

    /**************************************************
     * Get a chunk from the head of the free list.
     **************************************************/

    chunk = info->firstFree;

    if (chunk != -1)
    {
        info->firstFree       = info->freeList[chunk];
        info->freeList[chunk] = -2;

        info->availableBuffers--;

        FM_LOG_DEBUG(FM_LOG_CAT_BUFFER,
                     "Allocated buffer #%d, %d left\n", chunk,
                     info->availableBuffers);

    }


    DROP_PLAT_STATE_LOCK();

    ret = (chunk == -1) ? NULL : &info->table[chunk];

    /* set the next pointer of the allocated buffer to NULL */
    if (ret)
    {
        ret->next = NULL;
    }

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_BUFFER, ret, "%p\n", (void *) ret);

}   /* end fmPlatformAllocateBuffer */




/*****************************************************************************/
/** fmPlatformFreeBuffer
 * \ingroup platform
 *
 * \desc            Return a packet buffer, previously allocated with a
 *                  call to ''fmAllocateBuffer'', to the free buffer pool.
 *
 * \note            If the buffer is part of a chain of buffers, this function
 *                  will not dispose of the other buffers in the chain.  See
 *                  fmFreeBufferChain for disposing of an entire chain.
 *
 * \param[in]       buf points to the buffer's ''fm_buffer'' structure.
 *
 * \return          FM_OK
 *
 *****************************************************************************/
fm_status fmPlatformFreeBuffer(fm_buffer *buf)
{
    fm_bufferAllocState *info;
    fm_switch *          switchState;
    fm_int               switchNum;
    fm_int               index;

    index = buf->index;

    FM_LOG_ENTRY(FM_LOG_CAT_BUFFER,
                 "buf = %p, buf->index = %d\n",
                 (void *) buf, index);

    info = &fmRootPlatform->bufferAllocState;

    TAKE_PLAT_STATE_LOCK();

    /**************************************************
     * Validate chunk.
     **************************************************/

    /* Valid chunk index? */
    if ( (index >= info->totalBufferCount) ||
        (index == -1) ||
        (info->freeList[index] != -2) )
    {
        /* Invalid chunk index. */
        DROP_PLAT_STATE_LOCK();

        FM_LOG_EXIT(FM_LOG_CAT_BUFFER, FM_ERR_INVALID_ARGUMENT);
    }

    /**************************************************
     * Put the chunk back in the free list.
     **************************************************/

    info->freeList[index] = info->firstFree;
    info->firstFree       = index;

    /**************************************************
     * Reset its pointer to the base of the chunk
     **************************************************/

    info->table[index].data = GetBufferMemory(index);

    /**************************************************
     * Count the free event.
     **************************************************/

    info->availableBuffers++;

    FM_LOG_DEBUG(FM_LOG_CAT_BUFFER,
                 "Freed buffer #%d, %d left\n", index,
                 info->availableBuffers);

    DROP_PLAT_STATE_LOCK();

    /**************************************************
     * If any switch's frame receiver could previously
     * not get a chunk, then signal it to try again
     * now that there is one available.
     **************************************************/

    for (switchNum = FM_FIRST_FOCALPOINT ;
         switchNum <= FM_LAST_FOCALPOINT ;
         switchNum++)
    {
        switchState = fmRootApi->fmSwitchStateTable[switchNum];

        if (switchState && switchState->state == FM_SWITCH_STATE_UP)
        {
            if (switchState->buffersNeeded)
            {
                /* Clear out the flag since we know we have a free buffer */
                switchState->buffersNeeded      = FALSE;
                
                /**************************************************
                 * We must take a lock before writing
                 * intrReceivePackets because the lock is used
                 * by the API to ensure an atomic read-modify-write
                 * to intrReceivePackets.
                 *
                 * The platform lock is used instead of state lock
                 * because on FIBM platforms, there is an access
                 * to intrSendPackets that must be protected before
                 * the switch's locks are even created. 
                 **************************************************/
                
                FM_TAKE_PKT_INT_LOCK(switchNum);
                switchState->intrReceivePackets = TRUE;
                FM_DROP_PKT_INT_LOCK(switchNum);

                /* Wake up the interrupt handler so it will see the message. */
                fmPlatformTriggerInterrupt(switchNum, FM_INTERRUPT_SOURCE_API);
            }
        }
    }


    FM_LOG_EXIT(FM_LOG_CAT_BUFFER, FM_OK);

}   /* end fmPlatformFreeBuffer */




/*****************************************************************************/
/** fmPlatformGetAvailableBuffers
 * \ingroup intPlatform
 *
 * \desc            Returns the number of available buffers.
 *
 * \param[out]      count points to caller allocated storage where the number
 *                  of available buffers is written.
 *
 * \return          fm_status code
 *
 *****************************************************************************/
fm_status fmPlatformGetAvailableBuffers(fm_int *count)
{
    FM_LOG_ENTRY(FM_LOG_CAT_BUFFER, "count=%p\n", (void *) count);

    if (!count)
    {
        FM_LOG_FATAL(FM_LOG_CAT_BUFFER, "Count pointer is null\n");

        FM_LOG_EXIT(FM_LOG_CAT_BUFFER, FM_ERR_INVALID_ARGUMENT);
    }

    *count = fmRootPlatform->bufferAllocState.availableBuffers;

    FM_LOG_EXIT(FM_LOG_CAT_BUFFER, FM_OK);

}   /* end fmPlatformGetAvailableBuffers */
