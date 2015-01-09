# subroutines to rename to/from SPICE compatible syntax

package rename;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(&to_spice &list_to_spice &from_spice &list_from_spice);
}

# convert hierarchical name to lowercase hex ASCII format
sub to_spice {
    my ($node) = @_;
    return $node if ($node eq "0"); # never translate magical ground node!
    my $new = "";
    while ($node =~ s/(^[^\/]+)\///g) {
        $new .= "x" . string_to_hex($1) . ".";
    }
    if ($node =~ /.+/) {
        $new .= string_to_hex($node);
    }
    return $new;
}

# rename a list of nodes
sub list_to_spice {
    for (my $i=0; $i<@_; $i++) {
        $_[$i] = to_spice($_[$i]);
    }
}

# reverse translation of a list of nodes
sub from_spice {
    my ($node) = @_;
    my $new = "";
    while ($node =~ s/^x([^\.]*)\.//g) {
        $new .= hex_to_string($1) . ".";
    }
    $new .= hex_to_string($node);
    return $new;
}

# reverse translation of a list of nodes
sub list_from_spice {
    for (my $i=0; $i<@_; $i++) {
        $_[$i] = from_spice($_[$i]);
    }
}

# convert string to lowercase hex ASCII format
sub string_to_hex {
    my ($name) = @_;
    my $new = "h";
    my @chars = split("",$name);
    for (my $c=0; $c<@chars; $c++) {
        $new .= sprintf("%2x",ord($chars[$c]));
    }
    return $new;
}

# convert from lowercase ASCII format back to regular string
sub hex_to_string {
    my ($name) = @_;
    my $new = "";
    if ($name =~ s/^h//g) {
        my @chars = split("",$name);
        for (my $c=0; $c<@chars; $c+=2) {
            my $x = hex($chars[$c] . $chars[$c+1]);
            $new .= chr($x);
        }
        return $new;
    } else {
        return $name;
    }
}

1;
