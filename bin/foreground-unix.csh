#! /bin/csh -f

# This file is used to run VCODE from NESL in foreground.
# It works either for local or nonlocal machines.
#
# The argumets are:
#  1: rsh_command --- e.g. "rsh -l joeuser foo.uvwk.edu" 
#                       (can be empty, in which case it is run locally)
#  2: interp_file --- vcode interpreter executable
#  3: memory_size --- memory size (passed to the interpreter)
#  4: temp_dir    --- temporary directory where files are stored
#  5: job_name    --- the job_name (used to identify files)
#  6: max_time    --- currently ignored.	
#  7: arguments   --- any arguments (ignored)
#  
#  The rsh_command, interp_file, memory_size, temp_dir, max_time, 
#  and arguments are passed directly from the configuration
#  specification in NESL.

# If #1 (rsh_command) is empty, then it just leaves a blank space at
# the beginning of the line and $2 (interp_file) is executed directly.

set codefile = $4$5_code
set outfile = $4$5_out

$1 $2 -m $3 $codefile > $outfile
