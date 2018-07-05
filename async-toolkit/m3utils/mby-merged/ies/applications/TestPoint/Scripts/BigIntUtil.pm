
package Scripts::BigIntUtil;

use Math::BigInt;
use Scripts::Utilities;

sub setBit($$)
{
    validatePrototype(@_, 2, 2);
    my ($int, $bit) = @_;

    my $x = new Math::BigInt(1)->blsft($bit);
    return $int->copy()->bior($x);
}

sub clearBit($$)
{
    validatePrototype(@_, 2, 2);
    my ($int, $bit) = @_;

    # To figure out the bit width of $int:
    # convert to string, get length, subtract 2 for 0x
    # multiply by 4 to get number of bits
    my $width = (length($int->as_hex())-2)*4;
    my $mask = allOnes($width); 
    my $y = new Math::BigInt(1)->blsft($bit)->bneg()->bdec()->band($mask);
    return $int->copy()->band($y);
}

sub setBits($$)
{
    validatePrototype(@_, 2, 2);
    my ($int, $bits) = @_;

    my $copy = $int->copy();
    foreach my $bit (@{$bits})
    {
        my $x = new Math::BigInt(1)->blsft($bit);
        $copy->bior($x);
    }
    return $copy;
}

sub setInt($$;$$)
{
    my ($bint, $val, $offset, $numbits) = @_;

    if(!defined $offset) { $offset = 0; }
    if(!defined $numbits) { $numbits = 32; }

    my $x = new Math::BigInt($val);
    $x = $x->band(2**$numbits-1);
    $x = $x->blsft($offset);
    $x = $x->bior($bint);
    return $x; 
}

sub getBit($$)
{
    validatePrototype(@_, 2, 2);
    my ($int, $bit) = @_;
   
    my $x = $int->copy();
    $x->brsft($bit)->band(1);
    if($x == 1)
    {
        return 1;
    }else
    {
        return 0;
    }
}

sub getInt($$)
{
    validatePrototype(@_, 2, 2);

    my ($int, $start) = @_;

    my $x = $int->copy();

    $x->brsft($start)->band(0xffffffff);
    return $x->numify();
}

sub getSlice($$$)
{
    validatePrototype(@_, 3, 3);

    my ($int, $high, $low) = @_;

    my $width = 1 + $high - $low;
    my $mask = new Math::BigInt(0);
    $mask = setBits($mask, [0..($width-1)]);
    my $x = $int->copy();
    $x->brsft($low)->band($mask);
    return $x;
}

sub array2bigint
{
    my ($arr) = @_;
    my $b = Math::BigInt->new($arr->[@$arr-1]);

    for (my $i = @$arr-2; $i >= 0; $i--)
    {
        $b->blsft(32);
        $b->bior(Math::BigInt->new($arr->[$i]));
    }

    return $b;
}

sub bigint2array
{   
    my ($b) = @_;
    my @arr = ();

    $b = $b->copy();

    while(!$b->is_zero())
    {
        my $t = $b->copy();
        $t->band(0xffffffff);
        push(@arr, $t->as_number());
        $b->brsft(32);
    }

    # fill out the extra
    foreach my $tmp (0..7)
    {
        push(@arr, 0);
    }

    return \@arr;
}

# Given a bigInt width, generate a new randomzied BigInt
# Optionally, a bit probability can be provided.
#   25 = roughly 25% of bits will be set
#   75 = roughly 75% of bits will be set
#  default is 50
sub randomize($;$)
{
    my ($numbits, $bitProb) = @_;

    if(! defined $bitProb) { $bitProb = 50; }

    my $v = Math::BigInt->new(0);

    for(my $i = 0; $i < $numbits; $i++)
    {
        my $r = int(rand(100));
        if($r < $bitProb)
        {
            $v = setBit($v, $i);
        }
    }    
    return $v;
}

sub allOnes($)
{
    my ($len) = @_;
    my $b = Math::BigInt->bone();
    $b->blsft($len);
    $b->bsub(Math::BigInt->bone());
    return $b;
}

sub getNumBitsSet($)
{
    my ($b) = @_;

    my $cnt = 0;
    $b = $b->copy();
    while(!$b->is_zero())
    {
        if($b->is_odd())
        {
            $cnt++;
        }

        $b = $b->brsft(1);
    }
    return $cnt;
}
1;
