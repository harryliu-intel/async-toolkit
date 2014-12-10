#!/bin/csh

# setup some paths

setenv MY_TOOLS_DIR $HOME/toolhome
setenv DFII_CLIENT ${USER}-dfII

# settings per project, can be conditional if you wish

    setenv CAST_DIR $HOME/hw/cast
    setenv SPEC_DIR $HOME/hw/layout/tsmc13/spec
    setenv DFII_DIR $HOME/hw/layout/tsmc13/dfII/x8
    setenv CDS_DIR  $HOME/cds_wd

    setenv PROCESS tsmc13

# derive more env variables

setenv PDK fulcrum-${PROCESS}-pdk
setenv CAST_PATH "${CAST_DIR}:${SPEC_DIR}"
setenv CAST_OPTS "--cast-path=$CAST_PATH --cds-wd=$CDS_DIR --dfII-dir=$DFII_DIR --cast-version=2"
setenv CAST_SPEC_DFII "--cast-dir=$CAST_DIR --spec-dir=$SPEC_DIR --dfII-dir=$DFII_DIR"

# env variables for fulcrum script

setenv FULCRUM "/nfs/site/disks/local_tools/bin/fulcrum --pdk=$PDK"

setenv MY_FULCRUM "/nfs/site/disks/local_tools/bin/fulcrum --pdk=$PDK --toolhome=$MY_TOOLS_DIR --latest"

setenv BUILD_FULCRUM "/home/local/common/fulcrum/bin/buildfulcrum --toolhome=$MY_TOOLS_DIR --root-project-dir=$HOME/sw/cad --root-target-dir=$HOME/cadbuild --nop4sync --keep --overwrite"

setenv BUILD_FULCRUM_PDK "/home/local/common/fulcrum/bin/buildfulcrum --toolhome=$MY_TOOLS_DIR --root-project-dir=$HOME/pdk --root-target-dir=$HOME/cadbuildpdk --nop4sync --overwrite"

# aliases for commercial tools

alias layout  'cd $CDS_DIR; $FULCRUM ic icc assura layout'
alias lmstat  '$FULCRUM lmstat'
alias cdsdoc  '$FULCRUM cdsdoc'

# aliases for java tools

# verification aliases

alias simple '$HOME/verify-pv2/regression/scripts/simple --cast.path=$CAST_PATH --log.trans.enable=true'
alias regression '$HOME/verify-pv2/regression/scripts/regression --cast.path=$CAST_PATH --log.trans.enable=true'

# layout related tools

alias mkcdswd '$FULCRUM mkcdswd --target-dir=$CDS_DIR --dfII-dir=$DFII_DIR --fulcrum-pdk-root=$PDK_ROOT --cast-path=$CAST_PATH --user-template=/home/user/lines/cds_wd/$PROCESS --force'

# aliases for lve

alias lve '$FULCRUM lve $CAST_SPEC_DFII --qsub=1 --jobs=4'
alias mylve '$MY_FULCRUM lve $CAST_SPEC_DFII --qsub=1 --jobs=4'

# aliases for ubersize

alias ubersize '$FULCRUM ubersize $CAST_SPEC_DFII --layoutClient=$DFII_CLIENT --specClient=$CAST_CLIENT'
alias myubersize '$MY_FULCRUM ubersize $CAST_SPEC_DFII --layoutClient=$DFII_CLIENT --specClient=$CAST_CLIENT'

# fulcrum tool system aliases

alias myfulcrum '$MY_FULCRUM'

alias build           '$BUILD_FULCRUM'
alias build_ubersize  '$BUILD_FULCRUM --packages ubersize'
alias build_lve       '$BUILD_FULCRUM --packages lve'
alias build_vi       '$BUILD_FULCRUM --packages virtuoso-integration'
alias build_slacker   '$BUILD_FULCRUM --packages slacker'
alias build_synthesis '$BUILD_FULCRUM --packages synthesis'
alias build_pdk       '$BUILD_FULCRUM_PDK --packages fulcrum-${PDK}-pdk'
alias build_all       '$BUILD_FULCRUM; $BUILD_FULCRUM_PDK --packages fulcrum-nv90-pdk,fulcrum-tsmc13-pdk'
alias updatefmdb      '/home/local/common/fulcrum/bin/updatefmdb --toolhome $MY_TOOLS_DIR'i
