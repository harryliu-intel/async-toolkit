%StageParamMap = (
   # Run the Natural Docs flow only when this library is being created
  'bman.mby.vcs.vcs_createlib_mby_rtl_lib' => {
      stage_sub => {                         # keyword - user-code hooks go here
         ndocs => {                          # eg. 'fix-jem-hdl'; name is arbitrary, just needs to be unique
            function => "UserCode::ndocs",   # point to the exact perl package and subroutine to be executed
            location => "last",              # "<first|last> - add to the beginning ("first") or end ("last") of the list of hooks
            type => "pre_run",               # <setup|pre_run|post_run|shutdown>" - setup/pre_run are before main stage execution, post_run/shutdown after, pre_run/post_run are inside the strace (caching) boundary
            override => 0,                   # override previously declared subs of this type?
            onfail => "fail",                # "fail|ignore" - what to do if hook returns non-zero; fail the stage, or just ignore it
         },
      },
   },
);
