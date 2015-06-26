#Returns the number of words minus one in the specified string.
NUM_WORDS_MINUS_ONE = $(words $(wordlist 2,$(words $(1)),$(1)))

#Returns the specified string with the last word in the specified string
#removed.
STRIP_LAST_WORD = $(wordlist 1, $(call NUM_WORDS_MINUS_ONE,$(1)),$(1))

#Returns the last word of the specified string.
GET_LAST_WORD = $(wordlist $(words $(1)),$(words $(1)), $(1))

#Returns the specified string with the first word of the specified string removed.
STRIP_FIRST_WORD = $(wordlist 2,$(words $(1)),$(1))

#Returns the specified string with the first N words removed from the specified string.
#$(1)=N, $(2)=String to strip from
#Example: $(call STRIP_FIRST_N_WORDS,2,foo bar baz biz blabber) results in: baz biz blabber.
STRIP_FIRST_N_WORDS = $(call STRIP_FIRST_WORD,$(wordlist $(1),$(words $(2)),$(2)))

#-----------------------------------------Stack Functions------------------------------------
#The stack will the first parameter to any stack function.
#THe stack variable should always be set to the result of a stack function.

#push the specified value on to the specified stack.
#Each word in the value parameter will be treated as it's own stack frame, will have
#to be popped seperately.
#$(1)=Stack String, $(2) thing to push
PUSH_STACK = $(2) $(1)

#Pop the top of the specified stack.  The result of this function
#is the new value for the stack variable NOT the value popped.
#$(1)=Stack String
POP_STACK = $(call STRIP_FIRST_WORD,$(1))

#Returns the top of the specified stack.
TOP_OF_STACK = $(firstword $(1))

#----------------------------------------------------------------------------------

GET_PARENT_DIR = $(shell cd $(1); cd ..; echo $$PWD)

#Merges to words of the specified string into one word inserted the
#specified seperator between words.
#$(1) is the string to merge.
#$(2) is the seperator to insert between words.
#Example: $(call MERGE_WORDS,/mnt disks teton chrisb,/) results in: /mnt/disks/teton/chrisb
MERGE_WORDS = $(subst $(my_variable_with_a_space_in_it),$(2),$(strip $(1))) 
my_empty_variable :=
my_variable_with_a_space_in_it:=$(my_empty_variable) $(my_empty_variable)

TO_LOWERCASE = $(shell $(PERL) -e '{print lv("$1");}' )

#This function makes some attempt to cononicalize path string.  Right now
#it just logically resolves ../'s in the specified path string.

CONONICALIZE_PATH = $(shell echo '$(1)' | $(GNUSED) -e 's,/\+,/,g' -e '{:lab s,/[^/]\+/\.\.,,; t lab}' -e 's,.*//,/,g' )

# An implementation of CONONICALIZE_PATH that only use built-in functions found
# in make 3.81 and later
CANONICALIZE_PATH = $(if $(patsubst /%,,$(1)),$(patsubst /%,%,$(abspath /$(1))),$(abspath $(1)))

UNIQ = $(shell echo '$(1)' | $(PERL) -e 'map { print "$$_ " if 1 == ++$$s{$$_}} split(" ",<>)')

#Clean up a list of words such that there will be only a single space between each word.
CONONICALIZE_LIST = $(foreach entry,$(1),$(entry))

PARENTSOFDIRS = $(foreach dir,$(1), $(if $(filter $(dir)/%,$(2)),$(dir),))
