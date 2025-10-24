 {           
 #@BASE_CFG_FILES = (
 #                  );

 %PATTERNS_DEF = (
    Modes => {
            AceDefault_JG => {
                Ignore   => 0,    # Set this to ignore this mode
                Required => 0,    # MUST find this many instances of StartString
                StartString => '/^# JASPERGOLD EXECUTION START/'
                ,                 # regular-expression that activates this mode
                EndString => '/^# JASPERGOLD EXECUTION DONE/'
                ,    # regular-expressions that deactivates this mode
                 # Define a list of regular expressions which must all exist within the
                 #   start and end expressions each time the mode is activated.
                 # Note: An end string must be specified for RequiredText to be checked
                RequiredText => [
                    '/Properties Considered/',
                    '/- cex *:/',
                    '/- unreachable *:/',
                    '/- error *:/',
                    '/- undetermined *:/',
                ],

             # Define a list of regular expressions which are ignored within the
             # start and
             #  end expressions.
                okErrors => [

#                  '/Example IGNORE ERROR/',
                    '/ICD011.*has been changed to "error" level/',
                    '/WARNING.*WCK020.*/',
                ],
          },
        },
              Errors => [
                # FPV  (Note that items will apply to all tests, but regexps likely jasper-unique)
                [ '/^\[\d*\].*undetermined.*\s(\d|[1-3]\d)\s/', "ALL", 2 ]
                ,    # Bound <=39 is failure
                [ '/^\[\d*\].*cex/',         "ALL", 1 ],
                [ '/^\[\d*\].*unreachable/', "ALL", 1 ],
              ],

              TimeoutErrors => [
              ],
              FatalErrors => [
              ],
              Warnings => [
              ],
              TestType     => {
                                   Default => {
                                               regex   => undef,
                                               keyword => "Proc",
                                              },
                                   Assembly => {
                                                regex   => '/Reading test memory image/',
                                                keyword => "Asm",
                                               },
                               },
 );
};
