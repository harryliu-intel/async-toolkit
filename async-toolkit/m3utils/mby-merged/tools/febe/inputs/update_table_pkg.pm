use strict;
use warnings;

package update_table_pkg;
use File::Spec::Functions qw(catdir);
use ToolConfig;
use lib &catdir(&ToolConfig_get_tool_path('febe3'), 'lib');
use FEBE::Flow::Dependency;

sub update_tables {
    my ($cls) = @_;
    &FEBE::Flow::Dependency::add_stage_to_unit_table(
        'fev_lite',
        [
            {
                cfg_key => 'fev_lite',
                relevance => ['ip_unit_turnin',],
                type => 'stage',
                stage_name => 'FEBE::FlowBEE::FV',
                deps =>   [ 'build_blocksinfo', 'lintra_elab', ],
            },
        ],
    );
}

1;
