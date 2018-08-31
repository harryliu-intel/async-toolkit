/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */
/******************************************************************************
 * File:            sdk_perlcode.i
 * Creation Date:   Jun. 10, 2008
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2011 Intel Corporation. All Rights Reserved. 
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

#if defined(FM_SWIG_AUXILARY_MODULE)
%perlcode
%{
{
    use strict;
    use warnings;

    use Math::BigInt;

    no strict 'refs';
    my $export = \@{__PACKAGE__ . '::EXPORT'};
    my $symbolTable = \%{__PACKAGE__ . '::'};
    use strict 'refs';

    # Create a lookup table for all symbols that are exported by this package.
    my %export = map {my $k = $_; $k =~ s/^\$//; $k => $_} @{$export};
    SYMBOL: while (my ($symbol, $glob) = each(%{$symbolTable}))
    {
        # Prevent existing SDK symbols from being overridden.
        if (exists($SDK::{$symbol}))
        {
            next SYMBOL;
        }
        # Re-export all symbols exported by this package.
        if (exists($export{$symbol}))
        {
            push(@SDK::EXPORT, $export{$symbol});
        }
        # Allow all __PACKAGE__::foo packages to be accessed as SDK::foo.
        elsif ($symbol !~ m/\w+::/)
        {
            next SYMBOL;
        }
        # Create an alias for the current symbol within the SDK package.
        no strict 'refs';
        *{'SDK::' . $symbol} = $glob;
    }
}
%}
#else
%perlcode
%{
    use Math::BigInt;
%}
#endif /* FM_SWIG_AUXILARY_MODULE */

