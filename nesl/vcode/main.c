/*
* Copyright (c) 1992, 1993, 1994, 1995
* Carnegie Mellon University SCAL project:
* Guy Blelloch, Jonathan Hardwick, Jay Sipelstein, Marco Zagha
*
* All Rights Reserved.
*
* Permission to use, copy, modify and distribute this software and its
* documentation is hereby granted, provided that both the copyright
* notice and this permission notice appear in all copies of the
* software, derivative works or modified versions, and any portions
* thereof.
*
* CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
* CONDITION.  CARNEGIE MELLON AND THE SCAL PROJECT DISCLAIMS ANY 
* LIABILITY OF ANY KIND FOR ANY DAMAGES WHATSOEVER RESULTING FROM 
* THE USE OF THIS SOFTWARE.
*
* The SCAL project requests users of this software to return to 
*
*  Guy Blelloch				nesl-contribute@cs.cmu.edu
*  School of Computer Science
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3890
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "config.h"
#include "vcode.h"
#include "y.tab.h"
#include "symbol_table.h"
#include "link_list.h"
#include "parse.h"
#include "vstack.h"
#include "program.h"
#include "stack.h"
#include "check_args.h"
#include "rtstack.h"
#include "constant.h"
#include "io.h"

static void main_loop PROTO_((void));
static void initialize PROTO_((void));
static int init_pc PROTO_((void));

int lex_trace = 0;
int stack_trace = 0;
int command_trace = 0;
int value_trace = 0;
int runtime_trace = 0;
int program_dump = 0;
int link_trace = 0;
int timer = 0;
unsigned int vstack_size_init = 1000000;
int garbage_notify = 1;
int check_args = 0;
int debug_flag = 0;
int heap_trace = 0;
int abort_on_error = 0;

static void initialize_parse PROTO_((void))
{
	hash_table_init();
	link_list_init();
	prog_init();
}

static void usage PROTO_((void))
{
    _fprintf(stderr, "vinterp [-cgt] [-m <num>] [-r <num>] [-d <debug-switch>] file-name\n");
    _fprintf(stderr, "\t -c \t\t check arguments\n");
    _fprintf(stderr, "\t -d abort\t dump core on internal error\n");
    _fprintf(stderr, "\t -d lex\t\t lex trace\n");
    _fprintf(stderr, "\t -d link\t link trace\n");
    _fprintf(stderr, "\t -d program\t dump program parse\n");
    _fprintf(stderr, "\t -d runtime\t trace call stack\n");
    _fprintf(stderr, "\t -d command\t trace commands executed\n");
    _fprintf(stderr, "\t -d stack\t trace vector stack\n");
    _fprintf(stderr, "\t -d values\t trace vector stack values\n");
    _fprintf(stderr, "\t -d heap\t trace heap usage (* = free, # = scratch)\n");
    _fprintf(stderr, "\t -g\t\t notify on memory compaction\n");
    _fprintf(stderr, "\t -m <num>\t vector stack size in number of doubles (default %d)\n", vstack_size_init);
    _fprintf(stderr, "\t -t\t\t time program execution\n");

    vinterp_exit(1);
}

int main(argc, argv)
int argc;
char *argv[];
{
    extern char* optarg;
    extern int optind;
    extern int errno;
    extern __const char *__const sys_errlist[];
    /* extern char *sys_errlist[]; */
    int c;
    char *filename = NULL;
    FILE *file;
    extern int getopt PROTO_((int, char *const[], const char *));

    init_file_list();

#if MPI
    CVL_init(&argc, &argv);
#else
#if CM2
    CVL_init(0);
#else
#if CM5
    CVL_init();
#endif
#endif
#endif

    while ((c = getopt(argc, argv, "cd:gm:r:t")) != EOF) {
	switch (c) {
	    case 'c':
		check_args = 1;
		break;
	    case 'd': 	/* lots of debug options */
		if (strcmp(optarg, "lex") == 0)
		    lex_trace = 1;
		else if (strcmp(optarg, "stack") == 0)
		    stack_trace = 1;
		else if (strcmp(optarg, "command") == 0)
		    command_trace = 1;
		else if (strcmp(optarg, "values") == 0)
		    value_trace = 1;
		else if (strcmp(optarg, "runtime") == 0)
		    runtime_trace = 1;
		else if (strcmp(optarg, "program") == 0)
		    program_dump = 1;
		else if (strcmp(optarg, "link") == 0)
		    link_trace = 1;
		else if (strcmp(optarg, "heap") == 0)
		    heap_trace = 1;
		else if (strcmp(optarg, "abort") == 0)
		    abort_on_error = 1;
		else 
		    usage();
		debug_flag = 1;
		break;
	    case 't':	/* time it */
		timer = 1;
		break;
	    case 'm':	/* vstack size */
		if (! sscanf(optarg,"%u", &vstack_size_init))
		    usage();
		break;
	    case 'g': 
		garbage_notify = 1;
		break;

	    default:
		usage();
	}
    }

    /* read file name: should be only unparsed argument */
    if ((argc != optind + 1) || 
	((filename = argv[optind]) == NULL)) {
	_fprintf(stderr, "vinterp: Please supply file name.\n");
	usage();
    }
    file = fopen(filename, "r");
    if (file == NULL) {
	_fprintf(stderr, "vinterp: Error opening input file %s: %s\n",
			filename, sys_errlist[errno]);
	vinterp_exit (1);
    }
    initialize_parse();
    yyin = file;

    /* parse and link the VCODE routine */
    if (! (yyparse() == 0 && parse_error == 0 && do_link() == 0)) {
	_fprintf(stderr, "Parse failure\n");
	vinterp_exit (1);
    }
    if (program_dump)
	show_program();


    initialize();
    main_loop();

#if MPI | CM2 | CM5
    CVL_quit();
#endif

    (void) fclose(file);
    return 0;
}

static void initialize PROTO_((void))
{
    assert(ok_vcode_table());		/* This doesn't do anything yet */
    stack_init();
    vstack_init(vstack_size_init);
    rtstack_init();
    rtstack_push(TOP_LEVEL);	/* End of Program Marker */
    const_install();		/* put constants into memory */
}

/* main interpreter loop */
static void main_loop PROTO_((void))
{
    int pc;
    cvl_timer_t begin_time, 		/* used by -t flag */
	 	start_time, end_time;	/* used by START_TIMER / STOP_TIMER */

    pc = init_pc();		/* initialize program counter */

    if (timer)
	tgt_fos(&begin_time);

    /* continue to execute VCODE instructions until the run_time stack
     * indicates that we've returned to the top-level
     */
    while (pc != TOP_LEVEL) {
	prog_entry_t *instruction = program + pc;	/* current ins */
	int branch_taken = 0;		/* true if don't bump pc at end */

	/* print debugging info before command is executing */
	if (debug_flag) {
	    if (command_trace) {
		show_prog_entry(instruction, instruction->lineno);
	    }
	    if (stack_trace || value_trace) {		/* give a stack dump */
		show_stack();
	    }
	    if (value_trace) {
		_fprintf(stderr,"\t");
		show_stack_values(stderr);
	    }
	    if (heap_trace) {
		vb_print();
	    }
	}
	/* WARNING: the code in this if-else-if-else block is a mess. */

	if (instruction->vop >= COPY) {		/* non-statements */
	    switch (instruction->vop) {
		case COPY:
		    do_copy(instruction);
		    break;
		case POP:
		    do_pop(instruction);
		    break;
		case CPOP:
		    do_cpop(instruction);
		    break;
		case PAIR:
		    do_pair(instruction);
		    break;
		case UNPAIR:
		    do_unpair(instruction);
		    break;
		case CALL:
		    rtstack_push(pc+1);		/* return to next stmt */
		    pc = instruction->misc.branch;
		    if (pc == UNKNOWN_BRANCH) {
			_fprintf(stderr, "vinterp: internal error: UNKNOWN_BRANCH encountered.\n");
			vinterp_exit(1);
		    }
		    branch_taken = 1;
		    break;
		case RET:
		    pc = rtstack_pop();
		    branch_taken = 1;
		    break;
		case IF:
		    if (!do_cond(instruction)) {
			pc = instruction->misc.branch;
			branch_taken = 1;
		    }
		    break;
		case ELSE:
		    pc = instruction->misc.branch;
		    branch_taken = 1;
		    break;
		case CONST:
		    do_const(instruction);
		    break;
		case EXIT:
    		    abort_on_error = 0;		/* don't dump core */
		    vinterp_exit(0);
		    break;
		case READ:
		    do_read(instruction, stdin);
		    break;
		case WRITE:
		    do_write(instruction, stderr);	
		    break;
		case FOPEN:
		    do_fopen(instruction);
		    break;
		case FCLOSE:
		    do_fclose(instruction);
		    break;
		case FREAD:
		    do_fread(instruction);
		    break;
		case FREAD_CHAR:
		    do_fread_char(instruction);
		    break;
		case FWRITE:
		    do_fwrite(instruction);
		    break;
		case SPAWN:
		    do_spawn(instruction);
		    break;
		case START_TIMER:
		    tgt_fos(&start_time);
		    break;
		case STOP_TIMER: {		/* put value on stack */
		    double elapsed_time;	/* in seconds */
		    tgt_fos(&end_time);
		    elapsed_time = tdf_fos(&end_time, &start_time);

		    stack_push(1, Float, -1);	/* Put vector on stack */
		    assert_mem_size(rep_vud_scratch(1));
		    rep_vud(se_vb(TOS)->vector, 0, elapsed_time, 1, SCRATCH);
		    break;
		    }
		case SRAND: {
		    int srand_arg;
		    assert(se_vb(TOS)->type == Int);
		    assert(se_vb(TOS)->len == 1);
		    assert_mem_size(ext_vuz_scratch(1));
		    srand_arg = ext_vuz(se_vb(TOS)->vector, 0, 1, SCRATCH);
		    rnd_foz(srand_arg);
		    stack_pop(TOS);

		    /* SRAND returns a T.  just make one */
		    stack_push(1, Bool, NOTSEGD);
		    assert_mem_size(rep_vub_scratch(1)); 
		    rep_vub(se_vb(TOS)->vector, 0, 1, 1, SCRATCH);
		    break;
		}
		case FUNC:
		case ENDIF:
		default:
		    _fprintf(stderr, "vinterp: illegal instruction: line %d\n",
			    instruction->lineno);
		    vinterp_exit(1);
		    break;
	    }
	}
	/* Take care of some special cases here */
	else if (instruction->vopdes->cvl_desc == SegOp) {
	    stack_entry_t stack_args[MAX_ARGS];	/* args for call */
	    get_args(instruction, stack_args);
	    if (check_args) {
		if (args_ok(instruction, stack_args) == 0) {
		    _fprintf(stderr, "vinterp: aborting: failed argument check.\n");
		    vinterp_exit(1);
		}
	    }

	    switch (instruction->vop) {
		case MAKE_SEGDES: {
		    if (check_args && se_vb(TOS)->type != Int) {
			_fprintf(stderr, "vinterp: line %d: not applying MAKE_SEGDES to an integer vector.\n",
				instruction->lineno);
			vinterp_exit(1);
		    } else {
			/* lengths vector is on TOS */
			stack_entry_t lengths_se = TOS;

			/* Find length of result vector */
			int lengths_len = se_vb(lengths_se)->len;
			int vector_len;

			assert_mem_size(add_ruz_scratch(lengths_len));

			vector_len = add_ruz(se_vb(lengths_se)->vector,
					     lengths_len, SCRATCH);

			stack_push(lengths_len, Segdes, vector_len);

			assert_mem_size(mke_fov_scratch(vector_len, lengths_len));
			mke_fov(se_vb(TOS)->vector, se_vb(lengths_se)->vector,
				vector_len, lengths_len, SCRATCH);
			stack_pop(lengths_se);
		    }
		}
		break;
		case LENGTHS: {
		    if (se_vb(TOS)->type != Segdes) {
			_fprintf(stderr, "vinterp: line %d: applying LENGTHS to a non-segment descriptor.\n",
				instruction->lineno);
			vinterp_exit(1);
		    } else {
			/* segd is on top of stack */
			stack_entry_t segd = TOS;

			/* answer is same length as segd */
			stack_push(se_vb(segd)->len, Int, NOTSEGD);

			assert_mem_size(len_fos_scratch(se_vb(segd)->seg_len, se_vb(segd)->len));
			len_fos(se_vb(TOS)->vector,
				se_vb(segd)->vector,
				se_vb(segd)->seg_len,
				se_vb(segd)->len,
				SCRATCH);
			stack_pop(segd);
		    }
		}
		break;
		case LENGTH: {  /* use REPLACE to put scalar on stack */
		    if (se_vb(TOS)->type == Segdes) {
			_fprintf(stderr, "vinterp: line %d: applying LENGTH to a segment descriptor.\n",
				instruction->lineno);
			vinterp_exit(1);
		    } else {
			stack_entry_t vector_se = TOS;
			stack_push(1, Int, NOTSEGD);
			assert_mem_size(rep_vuz_scratch(se_vb(vector_se)->len));
			rep_vuz(se_vb(TOS)->vector, 
				0,
				se_vb(vector_se)->len,
				1,
				SCRATCH);
			stack_pop(vector_se);
		    }
		}
		break;
		case PACK: {
		    stack_entry_t vector_se = stack_args[0];
		    stack_entry_t flag_se = stack_args[1];
		    stack_entry_t segd_se = stack_args[2];
		    stack_entry_t len_se;
		    stack_entry_t dest_se;
		    stack_entry_t dest_segd_se;
		    int len;
		    void (*funct)();			/* cvl function */
		    int (*s_funct)();			/* scratch function */

		    /* length result of pk1 */
		    stack_push(se_vb(segd_se)->len, Int, NOTSEGD);
		    len_se = TOS;		

		    assert_mem_size(pk1_lev_scratch(se_vb(segd_se)->seg_len,
						    se_vb(segd_se)->len));

		    /* get the lengths vector of result */
		    pk1_lev(se_vb(len_se)->vector,
			    se_vb(flag_se)->vector,
			    se_vb(segd_se)->vector,
			    se_vb(segd_se)->seg_len,
			    se_vb(segd_se)->len,
			    SCRATCH);
		    
		    /* find length of pack result */
		    assert_mem_size(add_ruz_scratch(se_vb(len_se)->len));
		    len = add_ruz(se_vb(len_se)->vector, 
				  se_vb(len_se)->len, SCRATCH);

		    /* push the destination vector and segd */
		    stack_push(len, se_vb(vector_se)->type, NOTSEGD);
		    dest_se = TOS;
		    stack_push(se_vb(len_se)->len, Segdes, len);
		    dest_segd_se = TOS;

		    /* create segment descriptor for result */
		    assert_mem_size(mke_fov_scratch(len, se_vb(len_se)->len));
		    mke_fov(se_vb(dest_segd_se)->vector, 
			    se_vb(len_se)->vector, 
		    	    len, 
			    se_vb(len_se)->len, SCRATCH);

		    /* finish the pack */
		    funct = cvl_funct(instruction->vop, instruction->type);
		    s_funct = (int (*)())scratch_cvl_funct(instruction->vop, instruction->type);

		    assert_mem_size((*s_funct)(se_vb(segd_se)->seg_len, se_vb(segd_se)->len, se_vb(dest_segd_se)->seg_len, se_vb(dest_segd_se)->len));
		    (*funct)(se_vb(dest_se)->vector,
			     se_vb(vector_se)->vector,
			     se_vb(flag_se)->vector, 
			     se_vb(segd_se)->vector,
			     se_vb(segd_se)->seg_len,
			     se_vb(segd_se)->len,
			     se_vb(dest_segd_se)->vector,
			     se_vb(dest_segd_se)->seg_len,
			     se_vb(dest_segd_se)->len,
			     SCRATCH);
		    /* pop off the arg vectors */
		    stack_pop(len_se);
		    stack_pop(segd_se);
		    stack_pop(flag_se);
		    stack_pop(vector_se);
		    }
		    break;
		default: 
		    _fprintf(stderr, "vinterp: internal error: illegal SegOp in vcode_table.\n");
		    vinterp_exit(1);
	    }
	} else {
	    /* do a vector op */
	    vb_t *dest[MAX_OUT];		/* where answer will go */
	    stack_entry_t stack_args[MAX_ARGS];	/* args for call */
	    void (*funct)();			/* cvl function */

	    get_args(instruction, stack_args);

	    if (check_args) {
	        if (args_ok(instruction, stack_args) == 0) {
		    _fprintf(stderr, "vinterp: aborting: failed argument check.\n");
		    vinterp_exit(1);
		}
	    }

	    /* allocate room for result and scratch */
	    allocate_result(instruction, stack_args, dest);
	    assert_scratch(instruction, stack_args);

	    funct = cvl_funct(instruction->vop, instruction->type);
	    if (funct == NULL) {
		_fprintf(stderr, "vinterp: internal error: missing cvl_funct_list entry for %s, type %s.\n",
			instruction->vopdes->vopname, type_string(instruction->type));
		vinterp_exit(1);
	    }

	    /* Do the cvl function call.
	     * This is a big case statement to handle the various
	     * possibilities of argument order */
	    switch (instruction->vopdes->cvl_desc) {
		case Elwise1:
		    (*funct)(dest[0]->vector, 
			     se_vb(stack_args[0])->vector,
			     se_vb(stack_args[0])->len,
			     SCRATCH);
		    break;
		case Elwise2:
		    (*funct)(dest[0]->vector, 
			     se_vb(stack_args[0])->vector,
			     se_vb(stack_args[1])->vector,
		             se_vb(stack_args[0])->len,
			     SCRATCH);
		    break;
		case Elwise3:
		    (*funct)(dest[0]->vector, 
			     se_vb(stack_args[0])->vector,
			     se_vb(stack_args[1])->vector,
			     se_vb(stack_args[2])->vector,
		             se_vb(stack_args[0])->len,
			     SCRATCH);
		    break;
		case Scan:
		case Reduce:
		    (*funct)(dest[0]->vector,
			     se_vb(stack_args[0])->vector,
			     se_vb(stack_args[1])->vector,
			     se_vb(stack_args[1])->seg_len,
			     se_vb(stack_args[1])->len,
			     SCRATCH);
		    break;
		case Bpermute:
		    (*funct)(dest[0]->vector,
			     se_vb(stack_args[0])->vector, /* source */
			     se_vb(stack_args[1])->vector, /* index */
			     se_vb(stack_args[2])->vector, /* s_sgd */
			     se_vb(stack_args[2])->seg_len,
			     se_vb(stack_args[2])->len,
			     se_vb(stack_args[3])->vector, /* d_sgd */
			     se_vb(stack_args[3])->seg_len,
			     se_vb(stack_args[2])->len,
			     SCRATCH);
		    break;
		case Permute:
		    (*funct)(dest[0]->vector,
			     se_vb(stack_args[0])->vector, /* src */
			     se_vb(stack_args[1])->vector, /* index */
			     se_vb(stack_args[2])->vector, /* segd */
			     se_vb(stack_args[2])->seg_len,
			     se_vb(stack_args[2])->len,
			     SCRATCH);
		    break;
		case Fpermute:
		case Bfpermute:
		    (*funct)(dest[0]->vector,
			     se_vb(stack_args[0])->vector,  /* source */
			     se_vb(stack_args[1])->vector,  /* index */
			     se_vb(stack_args[2])->vector,  /* flags */
			     se_vb(stack_args[3])->vector,  /* s_sgd */
			     se_vb(stack_args[3])->seg_len,
			     se_vb(stack_args[3])->len,
			     se_vb(stack_args[4])->vector,  /* d_sgd */
			     se_vb(stack_args[4])->seg_len,
			     se_vb(stack_args[4])->len,
			     SCRATCH);
		    break;
		case Dpermute:
		    /* optimize storage for when default can be destroyed */
		    if (se_vb(stack_args[2])->count == 1) {
			vstack_pop(dest[0]);
			dest[0] = se_vb(TOS) = se_vb(stack_args[2]);
			se_vb(stack_args[2])->count++;
		    }
		    (*funct)(dest[0]->vector,
			     se_vb(stack_args[0])->vector,  /* source */
			     se_vb(stack_args[1])->vector,  /* index */
			     se_vb(stack_args[2])->vector,  /* default */
			     se_vb(stack_args[3])->vector,  /* s_sgd */
			     se_vb(stack_args[3])->seg_len,
			     se_vb(stack_args[3])->len,
			     se_vb(stack_args[4])->vector,  /* d_sgd */
			     se_vb(stack_args[4])->seg_len,
			     se_vb(stack_args[4])->len,
			     SCRATCH);
		    break;
		case Dfpermute:
		    /* optimize storage for when default can be destroyed */
		    if (se_vb(stack_args[3])->count == 1) {
			vstack_pop(dest[0]);
			dest[0] = se_vb(TOS) = se_vb(stack_args[2]);
			se_vb(stack_args[3])->count++;
		    }
		    (*funct)(dest[0]->vector,		  /* dest */
			     se_vb(stack_args[0])->vector,  /* source */
			     se_vb(stack_args[1])->vector,  /* index */
			     se_vb(stack_args[2])->vector,  /* flags */
			     se_vb(stack_args[3])->vector,  /* defaults */
			     se_vb(stack_args[4])->vector,  /* s_sgd */
			     se_vb(stack_args[4])->seg_len,
			     se_vb(stack_args[4])->len,
			     se_vb(stack_args[5])->vector,  /* d_sgd */
			     se_vb(stack_args[5])->seg_len,
			     se_vb(stack_args[5])->len,
			     SCRATCH);
		    break;
		case Extract:
		    (*funct)(dest[0]->vector,
			     se_vb(stack_args[0])->vector,  /* source */
			     se_vb(stack_args[1])->vector,  /* index */
			     se_vb(stack_args[2])->vector,  /* sgd */
			     se_vb(stack_args[2])->seg_len,
			     se_vb(stack_args[2])->len,
			     SCRATCH);
		    break;
		case Replace: {
		    /* CVL replace is destructive; vcode's is not.
		     * If the source arg has only one ref, and hence
		     * will be deallocated after the REPLACE, we can
		     * save memory and copy costs by just REPLACEing
		     * directly into the source.
		     * Otherwise, we must make a copy of first arg.
		     */
		    if (se_vb(stack_args[0])->count == 1) { /* only ref */
			se_vb(stack_args[0])->count ++;     /* so won't free */

			vstack_pop(dest[0]);	  /* free storage of dest */
 			/* fix stack */
			dest[0] = se_vb(TOS) = se_vb(stack_args[0]);   
		    } else {				  /* copy source */
			vec_p copy_to = dest[0]->vector; 
			int res_len = dest[0]->len;
			switch (dest[0]->type) {	/* switch on type */
			    case Int:
			      assert_mem_size(cpy_wuz_scratch(res_len));
			      cpy_wuz(copy_to, se_vb(stack_args[0])->vector, res_len, SCRATCH);
			      break;
			    case Bool:
			      assert_mem_size(cpy_wub_scratch(res_len));
			      cpy_wub(copy_to, se_vb(stack_args[0])->vector, res_len, SCRATCH);
			      break;
			    case Float:
			      assert_mem_size(cpy_wud_scratch(res_len));
			      cpy_wud(copy_to, se_vb(stack_args[0])->vector, res_len, SCRATCH);
			      break;
			    default:
			      _fprintf(stderr, "vinterp: illegal operand for REPLACE.\n");
			      vinterp_exit (1);
			}
		    }
		    (*funct)(dest[0]->vector,  
			     se_vb(stack_args[1])->vector,  /* index */
			     se_vb(stack_args[2])->vector,  /* value */
			     se_vb(stack_args[3])->vector,  /* sgd */
			     se_vb(stack_args[3])->seg_len,
			     se_vb(stack_args[3])->len,
			     SCRATCH);
		    break;
		}
		case Dist:
		    (*funct)(dest[0]->vector,  
			     se_vb(stack_args[0])->vector,
			     se_vb(stack_args[1])->vector,
			     se_vb(stack_args[1])->seg_len,
			     se_vb(stack_args[1])->len,
			     SCRATCH);
		    break;
		case Index:
		    ind_lez (dest[0]->vector, 			/* dest */ 
			     se_vb(stack_args[0])->vector,	/* init */
			     se_vb(stack_args[1])->vector,	/* stride */
			     se_vb(stack_args[2])->vector,	/* d_segd */
			     se_vb(stack_args[2])->seg_len,
			     se_vb(stack_args[2])->len,
			     SCRATCH);
		    break;
		case NotSupported:
		default:
		    _fprintf(stderr, "vinterp: line %d: Operation %s not supported.\n",
				     instruction->lineno, 
				     instruction->vopdes->vopname);
		    vinterp_exit(1);
		}

	    /* remove used up args */
	    pop_args(stack_args, instruction->vopdes->arg_num);
	    }
	/* done executing command */
	if (branch_taken) 			/* adjust the program counter */
	    branch_taken = 0;
	else pc++;

    } /* done with program */
    if (timer) {
	tgt_fos(&end_time);
	_fprintf(stderr, "elapsed time = %f seconds\n",
		tdf_fos(&end_time, &begin_time));
    }

    /* print out final results */
    show_stack_values(stdout);
}

/* initialize the program counter to location of main, or of first funct */
static int init_pc()
{
    hash_entry_t main_hash;

    /* figure out where to start */
    if (main_fn[0] == '\0') {
	_fprintf(stderr, "vinterp: internal error: init_pc(): no start function.\n");
	vinterp_exit(1);
    } 
    main_hash = hash_table_lookup(main_fn);
    if (main_hash == NULL) {
	_fprintf(stderr, "vinterp: start function %s not found.\n", main_fn);
	vinterp_exit(1);
    } 
    return (main_hash->prog_num);
}

/* exit routine.  this is called if the interpreter finds itself in an 
 * illegal configuration.  create a core dump if the user wants one.
 */
void vinterp_exit(status)
int status;
{
    if (abort_on_error) {
	_fprintf(stderr, "vinterp: dumping core\n");
	abort();
    } else exit(status);
}


void debug_show_stack ()
{
  show_stack_values(stdout);
}
