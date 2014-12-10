#!/usr/intel/bin/perl

$spec_file=$ARGV[0];
$dsn_file=$ARGV[1];

open spec, "<$spec_file" or die;
while(<spec>) {
    chop;
    if(!($_ =~ "^#")) {
        if(! ($_ =~ "^define")) {
            @fields = split(/ /);
            $componentClass = $fields[0];
            $instanceMap{$componentClass} = [];
            @components = @fields[1..$#fields];
            for $component (@components) {
                $classMap{$component} = $componentClass;
            }
        }
    }
}

open dsn, "<$dsn_file" or die;
while(<dsn>) {
    if(/[ \t]*\(constant image:(.*) (.*)\)/) {
        $instance = $1;
        $component = $2;
        $componentClass = $classMap{$component};
        if($componentClass) {
            push @{ $instanceMap{$componentClass} } , $instance;
        }
    }
}

for $componentClass (keys(%instanceMap)) {    
    print "define (standard_cell (image_type $componentClass))\n";
    @instances = @{ @instanceMap{$componentClass} };
    foreach $instance (@instances) {
        print "image_property $instance (type $componentClass)\n";
    }
}

open spec, "<$spec_file" or die;
while(<spec>) {
    if(!($_ =~ "^#")) {
        if($_ =~ "^define") {
            print $_;
        }
    }
}
