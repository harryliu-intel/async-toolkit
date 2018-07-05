# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/Utilities.pm
# Creation Date:    10/20/06
# Description:      Support routines for Test Engine 2 tests.
#
# INTEL CONFIDENTIAL
# Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
#
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors. Title to the Material remains with Intel
# Corporation or its suppliers and licensors. The Material contains trade
# secrets and proprietary and confidential information of Intel or its
# suppliers and licensors. The Material is protected by worldwide copyright
# and trade secret laws and treaty provisions. No part of the Material may
# be used, copied, reproduced, modified, published, uploaded, posted,
# transmitted, distributed, or disclosed in any way without Intel's prior
# express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or 
# delivery of the Materials, either expressly, by implication, inducement,
# estoppel or otherwise. Any license under such intellectual property rights
# must be express and approved by Intel in writing.
###############################################################################

package Scripts::Utilities;
use strict;

require Exporter;
use List::Util qw(max sum);
use Base::Const;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    arrayCompare
    arrayCopy
    arrayFind
    arrayShuffle
    arrayUnique
    byteSwap
    hashClone
    isArray
    isHash
    isObject
    isScalar
    pick
    traceback
    validatePrototype
);

##@method public bool arrayCompare(void &a1[], void &a2[])
# @param[in] a1 A Perl reference to an array
# @param[in] a2 A Perl reference to an array
# @return FM_TRUE if all elements of @a a1 and @a a2 are equal, FM_FALSE
# otherwise
sub arrayCompare($$)
{
    my ($a1, $a2) = @_;

    for (my $i=0 ; $i<max(scalar(@{$a1}), scalar(@{$a2})) ; $i++)
    {
        if ($a1->[$i] != $a2->[$i])
        {
            return FM_FALSE;
        }
    }
    return FM_TRUE;
}

##@method public int arrayCopy(void &destination[], void &source[])
# @param[in] destination A Perl reference to a Perl array
# @param[in] source A Perl reference to a Perl array
# @return FM_SUCCESS on success, FM_FAILURE otherwise
sub arrayCopy($$)
{
    my ($destination, $source) = @_;

    if (!isArray($destination) || !isArray($source))
    {
        return FM_FAILURE;
    }

    my $n = scalar(@{$source});
    for (my $i=0 ; $i<$n ; $i++)
    {
        $destination->[$i] = $source->[$i];
    }
    return FM_SUCCESS;
}

##@method public int[] arrayFind(void &x[])
# Determines the indices of the nonzero elements of the specified array
# @param[in] x A Perl array or a Perl reference to an array
# @return A Perl array containing the indices of the nonzero elements of the
# specified array
sub arrayFind(@)
{
    my (@array) = @_;

    my $x = (isArray($array[0]) ? $array[0] : [@array]);

    my @y = ();
    for (my $i=0 ; $i<scalar(@{$x}); $i++)
    {
        push(@y, $i) if ($x->[$i] != 0);
    }
    return @y;
}

##@method public void arrayShuffle(void &x[], int skip)
# Shuffles the elements of the specified array randomly
# @param[in,out] x A Perl reference to an array
# @param[in] skip A boolean indicating whether or not each element in the
# shuffled array should be different from its original value. By default @a
# skip is set to FM_FALSE
# @see Perl Cookbook Section 4.17
# @note Setting @a skip to FM_TRUE might result in considerable use of
# computing resources. Do not set this argument unless absolutely necessary
sub arrayShuffle($;$)
{
    my ($array, $skip) = @_;

    my $N = scalar(@{$array});
    if (defined($skip) && $skip)
    {
        my $copy = [];
        arrayCopy($copy, $array);
        do
        {
            no warnings 'syntax';
            arrayShuffle($array, FM_FALSE);
        } while (sum(map {int($array->[$_] == $copy->[$_])} (0 .. ($N - 1))));
    }
    else
    {
        for (my $i = $N; --$i; )
        {
            my $j = int(rand($i + 1));
            if ($i == $j)
            {
                next;
            }
            @{$array}[$i, $j] = @{$array}[$j, $i];
        }
    }
}

##@method public void[] arrayUnique(void &x[])
# Extracts the unique elements from the specified array
# @param[in] x A Perl array or a Perl reference to an array
# @return A Perl array containing the unique elements of the specified array
sub arrayUnique(@)
{
    my (@array) = @_;

    my $x = (isArray($array[0]) ? $array[0] : [@array]);

    my %y = ();
    foreach my $element (@{$x})
    {
        $y{$element}++;
    }
    return keys(%y);
}

##@method public int byteSwap(int x)
# Change the byte order of a 32-bit integer from little-endian to big-endian or
# from big-endian to little-endian
# @param[in] x The 32-bit integer value whose byte order will be changed
# @return The byte swapped version of @a x
sub byteSwap($)
{
    my ($x) = @_;

    return ((($x & 0xFF) << 24)
            | (($x & 0xFF00) << 8)
            | (($x & 0xFF0000) >> 8)
            | (($x & 0xFF000000) >> 24));
}

##@method public hash& hashClone(hash &original)
#
# @desc         Clones the specified Perl hash
#
# @param[in]    original A Perl reference to a Perl hash that is to be cloned
#
# @return       A Perl reference to a Perl hash containing the cloned copy
sub hashClone($)
{
    my ($original) = @_;

    my $copy = {};
    foreach my $key (keys(%{$original}))
    {
        if (isArray($original->{$key}))
        {
            $copy->{$key} = [];
            arrayCopy($copy->{$key}, $original->{$key});
        }
        elsif (isHash($original->{$key}))
        {
            no warnings 'syntax';
            $copy->{$key} = hashClone($original->{$key});
        }
        else
        {
            if ($original->{$key}->isa('Math::BigInt'))
            {
                $copy->{$key} = $original->{$key}->copy();
            }
            else
            {
                $copy->{$key} = $original->{$key};
            }
        }
    }
    return $copy;
}

##@method public bool isArray(void data)
# @param[in] data A Perl reference to a Perl variable
# @return FM_TRUE if @a data is a Perl array, FM_FALSE otherwise
sub isArray($)
{
   my ($data) = @_;

   return (ref($data) eq 'ARRAY' ? FM_TRUE : FM_FALSE);
}

##@method public bool isHash(void data)
# @param[in] data A Perl reference to a Perl variable
# @return FM_TRUE if @a data is a Perl hash, FM_FALSE otherwise
sub isHash($)
{
   my ($data) = @_;

   return (ref($data) eq 'HASH' ? FM_TRUE : FM_FALSE);
}

##@method public bool isObject(void *object, char *name)
# Verify whether @a object is a Perl object of the class pointed to by @a name
# @param object A Perl reference to a Perl variable
# @param name The name of a Perl class
# @return FM_TRUE if the specified variable is a Perl object of the class
# pointed to by @a name, FM_FALSE otherwise
sub isObject($$)
{
    my ($object, $name) = @_;

    return (ref($object) eq $name ? FM_TRUE : FM_FALSE);
}

##@method public bool isScalar(void data)
# @param[in] data A Perl reference to a Perl variable
# @return FM_TRUE if @a data is a Perl scalar, FM_FALSE otherwise
sub isScalar($)
{
   my ($data) = @_;

   return (ref($data) eq 'SCALAR' ? FM_TRUE : FM_FALSE);
}

##@method public scalar pick(void *array, void[] exclusions)
# Randomly picks and removes an element of the specified array
# @param[in,out] array A Perl reference to an array
# @param[in] exclusions @optional A Perl array containing elements that should
# be excluded from the picking process
# @return The randomly picked element
sub pick($;@)
{
    my ($array, @exclusions) = @_;

    my $element;
    if (defined($exclusions[0]))
    {
        my $N = scalar(@{$array});
        do
        {
            $element = int(rand($N));
        } while (sum(map {int($_ eq $array->[$element])} @exclusions) != 0
                 && $N > 1);
    }
    else
    {
        $element = int(rand(scalar(@{$array})));
    }
    return splice(@{$array}, $element, 1);
}

##@method public void traceback(char* message)
# Print an error message including a stack traceback to STDOUT
# @param[in] message The error message to print
sub traceback($)
{
    my ($message) = @_;

    my $indent = "  ";
    my $level;
    for ($level=1 ; ; $level++)
    {
        my ($package, $file, $line, $subroutine) = caller($level);
        if (!defined($subroutine)
            || ($subroutine eq "(eval)"))
        {
            last;
        }
        $file = pop(@{[split(/te2\/+/, $file)]});
        printf("%s%s at %s line %d:\n",
               join('', ($indent) x ($level - 1)), $subroutine, $file, $line);
    }
    printf("%s %s\n", join('', ($indent) x ($level - 1)), $message);
}

##@method public void validatePrototype(void *arguments, int m, int n)
# Verify at run-time whether a perl subroutine has been called with the correct
# amount of arguments
# @param[in] arguments The \@_ array
# @param[in] m The minimum amount of argument with which a subroutine is to be
# called
# @param[in] n The maximum amount of argument with which a subroutine is to be
# called. Set to @c -1 to signal that a subroutine can be called with an
# unlimited amount of arguments
sub validatePrototype(\@$;$)
{
    my ($arguments, $m, $n) = @_;

    $arguments = (ref($arguments) ? $arguments : [ $arguments ]);

    my ($package, $file, $line, $subroutine) = caller(1);
    if (scalar(@{$arguments}) < $m)
    {
        traceback(sprintf("Not enough arguments for %s", $subroutine));
        die;
    }
    elsif ((defined($n) ? $n != -1 : FM_TRUE)
            && scalar(@{$arguments}) > (defined($n) ? $n : $m))
    {
        traceback(sprintf("Too many arguments for %s", $subroutine));
        die;
    }
}

1;
