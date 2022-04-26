(in-package :cl-starpu)

(pkg-config-cflags "starpu-1.3")

(include "starpu.h")

(constant (#.(swig-lispify "ENODEV" 'constant) "ENODEV"))

(constant (#.(swig-lispify "STARPU_MAJOR_VERSION" 'constant) "STARPU_MAJOR_VERSION"))

(constant (#.(swig-lispify "STARPU_MINOR_VERSION" 'constant) "STARPU_MINOR_VERSION"))

(constant (#.(swig-lispify "STARPU_RELEASE_VERSION" 'constant) "STARPU_RELEASE_VERSION"))

(constant (#.(swig-lispify "STARPU_USE_CPU" 'constant) "STARPU_USE_CPU"))

(constant (#.(swig-lispify "STARPU_USE_OPENCL" 'constant) "STARPU_USE_OPENCL"))

(constant (#.(swig-lispify "STARPU_OPENMP" 'constant) "STARPU_OPENMP"))

(constant (#.(swig-lispify "STARPU_HAVE_VALGRIND_H" 'constant) "STARPU_HAVE_VALGRIND_H"))

(constant (#.(swig-lispify "STARPU_HAVE_MEMCHECK_H" 'constant) "STARPU_HAVE_MEMCHECK_H"))

(constant (#.(swig-lispify "STARPU_NON_BLOCKING_DRIVERS" 'constant) "STARPU_NON_BLOCKING_DRIVERS"))

(constant (#.(swig-lispify "STARPU_USE_MPI" 'constant) "STARPU_USE_MPI"))

(constant (#.(swig-lispify "STARPU_USE_MPI_MPI" 'constant) "STARPU_USE_MPI_MPI"))

(constant (#.(swig-lispify "STARPU_SYSTEM_BLAS" 'constant) "STARPU_SYSTEM_BLAS"))

(constant (#.(swig-lispify "STARPU_OPENGL_RENDER" 'constant) "STARPU_OPENGL_RENDER"))

(constant (#.(swig-lispify "STARPU_HAVE_X" 'constant) "STARPU_HAVE_X"))

(constant (#.(swig-lispify "STARPU_HAVE_POSIX_MEMALIGN" 'constant) "STARPU_HAVE_POSIX_MEMALIGN"))

(constant (#.(swig-lispify "STARPU_HAVE_MEMALIGN" 'constant) "STARPU_HAVE_MEMALIGN"))

(constant (#.(swig-lispify "STARPU_HAVE_MALLOC_H" 'constant) "STARPU_HAVE_MALLOC_H"))

(constant (#.(swig-lispify "STARPU_HAVE_SYNC_BOOL_COMPARE_AND_SWAP" 'constant) "STARPU_HAVE_SYNC_BOOL_COMPARE_AND_SWAP"))

(constant (#.(swig-lispify "STARPU_HAVE_SYNC_VAL_COMPARE_AND_SWAP" 'constant) "STARPU_HAVE_SYNC_VAL_COMPARE_AND_SWAP"))

(constant (#.(swig-lispify "STARPU_HAVE_SYNC_FETCH_AND_ADD" 'constant) "STARPU_HAVE_SYNC_FETCH_AND_ADD"))

(constant (#.(swig-lispify "STARPU_HAVE_SYNC_FETCH_AND_OR" 'constant) "STARPU_HAVE_SYNC_FETCH_AND_OR"))

(constant (#.(swig-lispify "STARPU_HAVE_SYNC_LOCK_TEST_AND_SET" 'constant) "STARPU_HAVE_SYNC_LOCK_TEST_AND_SET"))

(constant (#.(swig-lispify "STARPU_HAVE_SYNC_SYNCHRONIZE" 'constant) "STARPU_HAVE_SYNC_SYNCHRONIZE"))

(constant (#.(swig-lispify "STARPU_HAVE_FFTW" 'constant) "STARPU_HAVE_FFTW"))

(constant (#.(swig-lispify "STARPU_HAVE_FFTWF" 'constant) "STARPU_HAVE_FFTWF"))

(constant (#.(swig-lispify "STARPU_HAVE_FFTWL" 'constant) "STARPU_HAVE_FFTWL"))

(constant (#.(swig-lispify "STARPU_MAXNODES" 'constant) "STARPU_MAXNODES"))

(constant (#.(swig-lispify "STARPU_NMAXBUFS" 'constant) "STARPU_NMAXBUFS"))

(constant (#.(swig-lispify "STARPU_MAXCPUS" 'constant) "STARPU_MAXCPUS"))

(constant (#.(swig-lispify "STARPU_MAXNUMANODES" 'constant) "STARPU_MAXNUMANODES"))

(constant (#.(swig-lispify "STARPU_MAXCUDADEVS" 'constant) "STARPU_MAXCUDADEVS"))

(constant (#.(swig-lispify "STARPU_MAXOPENCLDEVS" 'constant) "STARPU_MAXOPENCLDEVS"))

(constant (#.(swig-lispify "STARPU_MAXMICDEVS" 'constant) "STARPU_MAXMICDEVS"))

(constant (#.(swig-lispify "STARPU_NMAXWORKERS" 'constant) "STARPU_NMAXWORKERS"))

(constant (#.(swig-lispify "STARPU_NMAX_SCHED_CTXS" 'constant) "STARPU_NMAX_SCHED_CTXS"))

(constant (#.(swig-lispify "STARPU_MAXIMPLEMENTATIONS" 'constant) "STARPU_MAXIMPLEMENTATIONS"))

(constant (#.(swig-lispify "STARPU_MAXMPKERNELS" 'constant) "STARPU_MAXMPKERNELS"))

(constant (#.(swig-lispify "STARPU_HAVE_GLPK_H" 'constant) "STARPU_HAVE_GLPK_H"))

(constant (#.(swig-lispify "STARPU_HAVE_LIBNUMA" 'constant) "STARPU_HAVE_LIBNUMA"))

(constant (#.(swig-lispify "STARPU_LINUX_SYS" 'constant) "STARPU_LINUX_SYS"))

(constant (#.(swig-lispify "STARPU_HAVE_SETENV" 'constant) "STARPU_HAVE_SETENV"))

(constant (#.(swig-lispify "STARPU_HAVE_UNSETENV" 'constant) "STARPU_HAVE_UNSETENV"))

(constant (#.(swig-lispify "STARPU_HAVE_UNISTD_H" 'constant) "STARPU_HAVE_UNISTD_H"))

(constant (#.(swig-lispify "STARPU_HAVE_HDF" 'constant) "STARPU_HAVE_HDF"))

(constant (#.(swig-lispify "STARPU_QUICK_CHECK" 'constant) "STARPU_QUICK_CHECK"))

(constant (#.(swig-lispify "STARPU_USE_DRAND" 'constant) "STARPU_USE_DRAND"))

(constant (#.(swig-lispify "STARPU_USE_ERAND" 'constant) "STARPU_USE_ERAND"))

(constant (#.(swig-lispify "STARPU_HAVE_NEARBYINTF" 'constant) "STARPU_HAVE_NEARBYINTF"))

(constant (#.(swig-lispify "STARPU_HAVE_RINTF" 'constant) "STARPU_HAVE_RINTF"))

(constant (#.(swig-lispify "STARPU_HAVE_HWLOC" 'constant) "STARPU_HAVE_HWLOC"))

(constant (#.(swig-lispify "STARPU_HAVE_PTHREAD_SPIN_LOCK" 'constant) "STARPU_HAVE_PTHREAD_SPIN_LOCK"))

(constant (#.(swig-lispify "STARPU_HAVE_PTHREAD_BARRIER" 'constant) "STARPU_HAVE_PTHREAD_BARRIER"))

(constant (#.(swig-lispify "STARPU_HAVE_PTHREAD_SETNAME_NP" 'constant) "STARPU_HAVE_PTHREAD_SETNAME_NP"))

(constant (#.(swig-lispify "STARPU_HAVE_STRUCT_TIMESPEC" 'constant) "STARPU_HAVE_STRUCT_TIMESPEC"))

(constant (#.(swig-lispify "STARPU_HAVE_HELGRIND_H" 'constant) "STARPU_HAVE_HELGRIND_H"))

(constant (#.(swig-lispify "HAVE_MPI_COMM_F" 'constant) "HAVE_MPI_COMM_F"))

(constant (#.(swig-lispify "STARPU_HAVE_CXX" 'constant) "STARPU_HAVE_CXX"))

(constant (#.(swig-lispify "STARPU_HAVE_STRERROR_R" 'constant) "STARPU_HAVE_STRERROR_R"))

(constant (#.(swig-lispify "STARPU_HAVE_STATEMENT_EXPRESSIONS" 'constant) "STARPU_HAVE_STATEMENT_EXPRESSIONS"))

(constant (#.(swig-lispify "STARPU_BACKTRACE_LENGTH" 'constant) "STARPU_BACKTRACE_LENGTH"))

(constant (#.(swig-lispify "STARPU_ACQUIRE_NO_NODE" 'constant) "STARPU_ACQUIRE_NO_NODE"))

(constant (#.(swig-lispify "STARPU_ACQUIRE_NO_NODE_LOCK_ALL" 'constant) "STARPU_ACQUIRE_NO_NODE_LOCK_ALL"))

(constant (#.(swig-lispify "STARPU_DISK_SIZE_MIN" 'constant) "STARPU_DISK_SIZE_MIN"))

(constant (#.(swig-lispify "STARPU_COO_GET_OFFSET" 'constant) "STARPU_COO_GET_OFFSET"))

(constant (#.(swig-lispify "STARPU_CSR_GET_OFFSET" 'constant) "STARPU_CSR_GET_OFFSET"))

(constant (#.(swig-lispify "STARPU_BCSR_GET_OFFSET" 'constant) "STARPU_BCSR_GET_OFFSET"))

(constant (#.(swig-lispify "STARPU_MALLOC_PINNED" 'constant) "STARPU_MALLOC_PINNED"))

(constant (#.(swig-lispify "STARPU_MALLOC_COUNT" 'constant) "STARPU_MALLOC_COUNT"))

(constant (#.(swig-lispify "STARPU_MALLOC_NORECLAIM" 'constant) "STARPU_MALLOC_NORECLAIM"))

(constant (#.(swig-lispify "STARPU_MEMORY_WAIT" 'constant) "STARPU_MEMORY_WAIT"))

(constant (#.(swig-lispify "STARPU_MEMORY_OVERFLOW" 'constant) "STARPU_MEMORY_OVERFLOW"))

(constant (#.(swig-lispify "STARPU_MALLOC_SIMULATION_FOLDED" 'constant) "STARPU_MALLOC_SIMULATION_FOLDED"))

(constant (#.(swig-lispify "STARPU_NOWHERE" 'constant) "STARPU_NOWHERE"))

(constant (#.(swig-lispify "STARPU_CPU" 'constant) "STARPU_CPU"))

(constant (#.(swig-lispify "STARPU_CUDA" 'constant) "STARPU_CUDA"))

(constant (#.(swig-lispify "STARPU_OPENCL" 'constant) "STARPU_OPENCL"))

(constant (#.(swig-lispify "STARPU_MIC" 'constant) "STARPU_MIC"))

(constant (#.(swig-lispify "STARPU_MPI_MS" 'constant) "STARPU_MPI_MS"))

(constant (#.(swig-lispify "STARPU_CODELET_SIMGRID_EXECUTE" 'constant) "STARPU_CODELET_SIMGRID_EXECUTE"))

(constant (#.(swig-lispify "STARPU_CODELET_SIMGRID_EXECUTE_AND_INJECT" 'constant) "STARPU_CODELET_SIMGRID_EXECUTE_AND_INJECT"))

(constant (#.(swig-lispify "STARPU_CODELET_NOPLANS" 'constant) "STARPU_CODELET_NOPLANS"))

(constant (#.(swig-lispify "STARPU_CUDA_ASYNC" 'constant) "STARPU_CUDA_ASYNC"))

(constant (#.(swig-lispify "STARPU_OPENCL_ASYNC" 'constant) "STARPU_OPENCL_ASYNC"))

(constant (#.(swig-lispify "STARPU_MAIN_RAM" 'constant) "STARPU_MAIN_RAM"))

(constant (#.(swig-lispify "STARPU_VARIABLE_NBUFFERS" 'constant) "STARPU_VARIABLE_NBUFFERS"))

(constant (#.(swig-lispify "STARPU_SPECIFIC_NODE_LOCAL" 'constant) "STARPU_SPECIFIC_NODE_LOCAL"))

(constant (#.(swig-lispify "STARPU_SPECIFIC_NODE_CPU" 'constant) "STARPU_SPECIFIC_NODE_CPU"))

(constant (#.(swig-lispify "STARPU_SPECIFIC_NODE_SLOW" 'constant) "STARPU_SPECIFIC_NODE_SLOW"))

(constant (#.(swig-lispify "STARPU_SPECIFIC_NODE_FAST" 'constant) "STARPU_SPECIFIC_NODE_FAST"))

(constant (#.(swig-lispify "STARPU_TASK_TYPE_NORMAL" 'constant) "STARPU_TASK_TYPE_NORMAL"))

(constant (#.(swig-lispify "STARPU_TASK_TYPE_INTERNAL" 'constant) "STARPU_TASK_TYPE_INTERNAL"))

(constant (#.(swig-lispify "STARPU_TASK_TYPE_DATA_ACQUIRE" 'constant) "STARPU_TASK_TYPE_DATA_ACQUIRE"))

(constant (#.(swig-lispify "STARPU_MODE_SHIFT" 'constant) "STARPU_MODE_SHIFT"))

(constant (#.(swig-lispify "STARPU_VALUE" 'constant) "STARPU_VALUE"))

(constant (#.(swig-lispify "STARPU_CALLBACK" 'constant) "STARPU_CALLBACK"))

(constant (#.(swig-lispify "STARPU_CALLBACK_WITH_ARG" 'constant) "STARPU_CALLBACK_WITH_ARG"))

(constant (#.(swig-lispify "STARPU_CALLBACK_ARG" 'constant) "STARPU_CALLBACK_ARG"))

(constant (#.(swig-lispify "STARPU_PRIORITY" 'constant) "STARPU_PRIORITY"))

(constant (#.(swig-lispify "STARPU_EXECUTE_ON_NODE" 'constant) "STARPU_EXECUTE_ON_NODE"))

(constant (#.(swig-lispify "STARPU_EXECUTE_ON_DATA" 'constant) "STARPU_EXECUTE_ON_DATA"))

(constant (#.(swig-lispify "STARPU_DATA_ARRAY" 'constant) "STARPU_DATA_ARRAY"))

(constant (#.(swig-lispify "STARPU_DATA_MODE_ARRAY" 'constant) "STARPU_DATA_MODE_ARRAY"))

(constant (#.(swig-lispify "STARPU_TAG" 'constant) "STARPU_TAG"))

(constant (#.(swig-lispify "STARPU_HYPERVISOR_TAG" 'constant) "STARPU_HYPERVISOR_TAG"))

(constant (#.(swig-lispify "STARPU_FLOPS" 'constant) "STARPU_FLOPS"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX" 'constant) "STARPU_SCHED_CTX"))

(constant (#.(swig-lispify "STARPU_PROLOGUE_CALLBACK" 'constant) "STARPU_PROLOGUE_CALLBACK"))

(constant (#.(swig-lispify "STARPU_PROLOGUE_CALLBACK_ARG" 'constant) "STARPU_PROLOGUE_CALLBACK_ARG"))

(constant (#.(swig-lispify "STARPU_PROLOGUE_CALLBACK_POP" 'constant) "STARPU_PROLOGUE_CALLBACK_POP"))

(constant (#.(swig-lispify "STARPU_PROLOGUE_CALLBACK_POP_ARG" 'constant) "STARPU_PROLOGUE_CALLBACK_POP_ARG"))

(constant (#.(swig-lispify "STARPU_EXECUTE_ON_WORKER" 'constant) "STARPU_EXECUTE_ON_WORKER"))

(constant (#.(swig-lispify "STARPU_EXECUTE_WHERE" 'constant) "STARPU_EXECUTE_WHERE"))

(constant (#.(swig-lispify "STARPU_TAG_ONLY" 'constant) "STARPU_TAG_ONLY"))

(constant (#.(swig-lispify "STARPU_POSSIBLY_PARALLEL" 'constant) "STARPU_POSSIBLY_PARALLEL"))

(constant (#.(swig-lispify "STARPU_WORKER_ORDER" 'constant) "STARPU_WORKER_ORDER"))

(constant (#.(swig-lispify "STARPU_NODE_SELECTION_POLICY" 'constant) "STARPU_NODE_SELECTION_POLICY"))

(constant (#.(swig-lispify "STARPU_NAME" 'constant) "STARPU_NAME"))

(constant (#.(swig-lispify "STARPU_CL_ARGS" 'constant) "STARPU_CL_ARGS"))

(constant (#.(swig-lispify "STARPU_CL_ARGS_NFREE" 'constant) "STARPU_CL_ARGS_NFREE"))

(constant (#.(swig-lispify "STARPU_TASK_DEPS_ARRAY" 'constant) "STARPU_TASK_DEPS_ARRAY"))

(constant (#.(swig-lispify "STARPU_TASK_COLOR" 'constant) "STARPU_TASK_COLOR"))

(constant (#.(swig-lispify "STARPU_HANDLES_SEQUENTIAL_CONSISTENCY" 'constant) "STARPU_HANDLES_SEQUENTIAL_CONSISTENCY"))

(constant (#.(swig-lispify "STARPU_TASK_SYNCHRONOUS" 'constant) "STARPU_TASK_SYNCHRONOUS"))

(constant (#.(swig-lispify "STARPU_TASK_END_DEPS_ARRAY" 'constant) "STARPU_TASK_END_DEPS_ARRAY"))

(constant (#.(swig-lispify "STARPU_TASK_END_DEP" 'constant) "STARPU_TASK_END_DEP"))

(constant (#.(swig-lispify "STARPU_TASK_WORKERIDS" 'constant) "STARPU_TASK_WORKERIDS"))

(constant (#.(swig-lispify "STARPU_SEQUENTIAL_CONSISTENCY" 'constant) "STARPU_SEQUENTIAL_CONSISTENCY"))

(constant (#.(swig-lispify "STARPU_TASK_PROFILING_INFO" 'constant) "STARPU_TASK_PROFILING_INFO"))

(constant (#.(swig-lispify "STARPU_TASK_NO_SUBMITORDER" 'constant) "STARPU_TASK_NO_SUBMITORDER"))

(constant (#.(swig-lispify "STARPU_CALLBACK_ARG_NFREE" 'constant) "STARPU_CALLBACK_ARG_NFREE"))

(constant (#.(swig-lispify "STARPU_CALLBACK_WITH_ARG_NFREE" 'constant) "STARPU_CALLBACK_WITH_ARG_NFREE"))

(constant (#.(swig-lispify "STARPU_PROLOGUE_CALLBACK_ARG_NFREE" 'constant) "STARPU_PROLOGUE_CALLBACK_ARG_NFREE"))

(constant (#.(swig-lispify "STARPU_PROLOGUE_CALLBACK_POP_ARG_NFREE" 'constant) "STARPU_PROLOGUE_CALLBACK_POP_ARG_NFREE"))

(constant (#.(swig-lispify "STARPU_TASK_SCHED_DATA" 'constant) "STARPU_TASK_SCHED_DATA"))

(constant (#.(swig-lispify "STARPU_SHIFTED_MODE_MAX" 'constant) "STARPU_SHIFTED_MODE_MAX"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_POLICY_NAME" 'constant) "STARPU_SCHED_CTX_POLICY_NAME"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_POLICY_STRUCT" 'constant) "STARPU_SCHED_CTX_POLICY_STRUCT"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_POLICY_MIN_PRIO" 'constant) "STARPU_SCHED_CTX_POLICY_MIN_PRIO"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_POLICY_MAX_PRIO" 'constant) "STARPU_SCHED_CTX_POLICY_MAX_PRIO"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_HIERARCHY_LEVEL" 'constant) "STARPU_SCHED_CTX_HIERARCHY_LEVEL"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_NESTED" 'constant) "STARPU_SCHED_CTX_NESTED"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_AWAKE_WORKERS" 'constant) "STARPU_SCHED_CTX_AWAKE_WORKERS"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_POLICY_INIT" 'constant) "STARPU_SCHED_CTX_POLICY_INIT"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_USER_DATA" 'constant) "STARPU_SCHED_CTX_USER_DATA"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_CUDA_NSMS" 'constant) "STARPU_SCHED_CTX_CUDA_NSMS"))

(constant (#.(swig-lispify "STARPU_SCHED_CTX_SUB_CTXS" 'constant) "STARPU_SCHED_CTX_SUB_CTXS"))

(constant (#.(swig-lispify "STARPU_DEFAULT_PRIO" 'constant) "STARPU_DEFAULT_PRIO"))

(constant (#.(swig-lispify "STARPU_PROFILING_DISABLE" 'constant) "STARPU_PROFILING_DISABLE"))

(constant (#.(swig-lispify "STARPU_PROFILING_ENABLE" 'constant) "STARPU_PROFILING_ENABLE"))

(constant (#.(swig-lispify "STARPU_NS_PER_S" 'constant) "STARPU_NS_PER_S"))

(constant (#.(swig-lispify "STARPU_FXT_MAX_FILES" 'constant) "STARPU_FXT_MAX_FILES"))

(constant (#.(swig-lispify "STARPU_THREAD_ACTIVE" 'constant) "STARPU_THREAD_ACTIVE"))

(cenum #.(swig-lispify "starpu_data_access_mode" 'enumname)
       ((:none "STARPU_NONE"))
       ((:r "STARPU_R"))
       ((:w "STARPU_W"))
       ((:rw "STARPU_RW"))
       ((:scratch "STARPU_SCRATCH"))
       ((:redux "STARPU_REDUX"))
       ((:commute "STARPU_COMMUTE"))
       ((:ssend "STARPU_SSEND"))
       ((:locality "STARPU_LOCALITY"))
       ((:access-mode-max "STARPU_ACCESS_MODE_MAX")))

(cenum #.(swig-lispify "starpu_data_interface_id" 'enumname)
       ((:unknown-interface-id "STARPU_UNKNOWN_INTERFACE_ID"))
       ((:matrix-interface-id "STARPU_MATRIX_INTERFACE_ID"))
       ((:block-interface-id "STARPU_BLOCK_INTERFACE_ID"))
       ((:vector-interface-id "STARPU_VECTOR_INTERFACE_ID"))
       ((:csr-interface-id "STARPU_CSR_INTERFACE_ID"))
       ((:bcsr-interface-id "STARPU_BCSR_INTERFACE_ID"))
       ((:variable-interface-id "STARPU_VARIABLE_INTERFACE_ID"))
       ((:void-interface-id "STARPU_VOID_INTERFACE_ID"))
       ((:multiformat-interface-id "STARPU_MULTIFORMAT_INTERFACE_ID"))
       ((:coo-interface-id "STARPU_COO_INTERFACE_ID"))
       ((:max-interface-id "STARPU_MAX_INTERFACE_ID")))

(cenum #.(swig-lispify "starpu_codelet_type" 'enumname)
       ((:seq "STARPU_SEQ"))
       ((:spmd "STARPU_SPMD"))
       ((:forkjoin "STARPU_FORKJOIN")))

(cenum #.(swig-lispify "starpu_task_status" 'enumname)
       ((:task-invalid "STARPU_TASK_INVALID"))
       ((:task-blocked "STARPU_TASK_BLOCKED"))
       ((:task-ready "STARPU_TASK_READY"))
       ((:task-running "STARPU_TASK_RUNNING"))
       ((:task-finished "STARPU_TASK_FINISHED"))
       ((:task-blocked-on-tag "STARPU_TASK_BLOCKED_ON_TAG"))
       ((:task-blocked-on-task "STARPU_TASK_BLOCKED_ON_TASK"))
       ((:task-blocked-on-data "STARPU_TASK_BLOCKED_ON_DATA"))
       ((:task-stopped "STARPU_TASK_STOPPED")))

(cenum #.(swig-lispify "starpu_node_kind" 'enumname)
       ((:unused "STARPU_UNUSED"))
       ((:cpu-ram "STARPU_CPU_RAM"))
       ((:cuda-ram "STARPU_CUDA_RAM"))
       ((:opencl-ram "STARPU_OPENCL_RAM"))
       ((:disk-ram "STARPU_DISK_RAM"))
       ((:mic-ram "STARPU_MIC_RAM"))
       ((:mpi-ms-ram "STARPU_MPI_MS_RAM")))

(cenum #.(swig-lispify "starpu_worker_archtype" 'enumname)
       ((:cpu-worker "STARPU_CPU_WORKER"))
       ((:cuda-worker "STARPU_CUDA_WORKER"))
       ((:opencl-worker "STARPU_OPENCL_WORKER"))
       ((:mic-worker "STARPU_MIC_WORKER"))
       ((:mpi-ms-worker "STARPU_MPI_MS_WORKER"))
       ((:any-worker "STARPU_ANY_WORKER")))

(cenum #.(swig-lispify "starpu_worker_collection_type" 'enumname)
       ((:worker-tree "STARPU_WORKER_TREE"))
       ((:worker-list "STARPU_WORKER_LIST")))

(cenum #.(swig-lispify "starpu_perfmodel_type" 'enumname)
       ((:perfmodel-invalid "STARPU_PERFMODEL_INVALID"))
       ((:per-arch "STARPU_PER_ARCH"))
       ((:common "STARPU_COMMON"))
       ((:history-based "STARPU_HISTORY_BASED"))
       ((:regression-based "STARPU_REGRESSION_BASED"))
       ((:nl-regression-based "STARPU_NL_REGRESSION_BASED"))
       ((:multiple-regression-based "STARPU_MULTIPLE_REGRESSION_BASED")))

(cstruct #.(swig-lispify "starpu_disk_ops" 'classname) "struct starpu_disk_ops")

(cstruct #.(swig-lispify "starpu_data_copy_methods" 'classname) "struct starpu_data_copy_methods")

(cstruct #.(swig-lispify "starpu_data_interface_ops" 'classname) "struct starpu_data_interface_ops")

(cstruct #.(swig-lispify "starpu_matrix_interface" 'classname) "struct starpu_matrix_interface")

(cstruct #.(swig-lispify "starpu_coo_interface" 'classname) "struct starpu_coo_interface")

(cstruct #.(swig-lispify "starpu_block_interface" 'classname) "struct starpu_block_interface")

(cstruct #.(swig-lispify "starpu_vector_interface" 'classname) "struct starpu_vector_interface")

(cstruct #.(swig-lispify "starpu_variable_interface" 'classname) "struct starpu_variable_interface")

(cstruct #.(swig-lispify "starpu_csr_interface" 'classname) "struct starpu_csr_interface")

(cstruct #.(swig-lispify "starpu_bcsr_interface" 'classname) "struct starpu_bcsr_interface")

(cstruct #.(swig-lispify "starpu_multiformat_data_interface_ops" 'classname) "struct starpu_multiformat_data_interface_ops")

(cstruct #.(swig-lispify "starpu_multiformat_interface" 'classname) "struct starpu_multiformat_interface")

(cstruct #.(swig-lispify "starpu_data_filter" 'classname) "struct starpu_data_filter")

(cstruct #.(swig-lispify "starpu_codelet" 'classname) "struct starpu_codelet"
         (#.(swig-lispify "where" 'slotname) "where" :type :uint32)
         (#.(swig-lispify "can_execute" 'slotname) "can_execute" :type :pointer)
         (#.(swig-lispify "type" 'slotname) "type" :type #.(swig-lispify "starpu_codelet_type" 'enumname))
         (#.(swig-lispify "max_parallelism" 'slotname) "max_parallelism" :type :int)
         (#.(swig-lispify "cpu_funcs" 'slotname) "cpu_funcs" :type :pointer :count STARPU_MAXIMPLEMENTATIONS)
         (#.(swig-lispify "cuda_funcs" 'slotname) "cuda_funcs" :type :pointer :count STARPU_MAXIMPLEMENTATIONS)
         (#.(swig-lispify "cuda_flags" 'slotname) "cuda_flags" :type :char :count STARPU_MAXIMPLEMENTATIONS)
         (#.(swig-lispify "opencl_funcs" 'slotname) "opencl_funcs" :type :pointer :count STARPU_MAXIMPLEMENTATIONS)
         (#.(swig-lispify "opencl_flags" 'slotname) "opencl_flags" :type :char :count STARPU_MAXIMPLEMENTATIONS)
         (#.(swig-lispify "nbuffers" 'slotname) "nbuffers" :type :int)
         (#.(swig-lispify "modes" 'slotname) "modes" :type (:pointer #.(swig-lispify "starpu_data_access_mode" 'enumname)) :count STARPU_NMAXBUFS)
         (#.(swig-lispify "dyn_modes" 'slotname) "dyn_modes" :type (:pointer #.(swig-lispify "starpu_data_access_mode" 'enumname)))
         (#.(swig-lispify "specific_nodes" 'slotname) "specific_nodes" :type :unsigned-int)
         (#.(swig-lispify "nodes" 'slotname) "nodes" :type :int :count STARPU_NMAXBUFS)
         (#.(swig-lispify "nodes" 'slotname) "dyn_nodes" :type (:pointer :int))
         (#.(swig-lispify "model" 'slotname) "model" :type (:pointer #.(swig-lispify "starpu_perfmodel_type" 'enumname)))
         (#.(swig-lispify "energy_model" 'slotname) "energy_model" :type (:pointer #.(swig-lispify "starpu_perfmodel_type" 'enumname)))
         (#.(swig-lispify "name" 'slotname) "name" :type (:pointer :char))
         (#.(swig-lispify "color" 'slotname) "color" :type :unsigned-int)
         (#.(swig-lispify "flags" 'slotname) "flags" :type :int))

(cstruct #.(swig-lispify "starpu_data_descr" 'classname) "struct starpu_data_descr")

(cstruct #.(swig-lispify "starpu_task" 'classname) "struct starpu_task"
         (#.(swig-lispify "name" 'slotname) "name" :type (:pointer :char))
         (#.(swig-lispify "cl" 'slotname) "cl" :type :pointer)
         (#.(swig-lispify "where" 'slotname) "where" :type :int32)
         (#.(swig-lispify "nbuffers" 'slotname) "nbuffers" :type :int)
         (#.(swig-lispify "dyn_handles" 'slotname) "dyn_handles" :type :pointer)
         (#.(swig-lispify "dyn_interfaces" 'slotname) "dyn_interfaces" :type :pointer)
         (#.(swig-lispify "dyn_modes" 'slotname) "dyn_modes" :type :pointer)
         (#.(swig-lispify "handles" 'slotname) "handles" :type :pointer :count STARPU_NMAXBUFS)
         (#.(swig-lispify "interfaces" 'slotname) "interfaces" :type :pointer :count STARPU_NMAXBUFS)
         (#.(swig-lispify "modes" 'slotname) "modes" :type (:pointer #.(swig-lispify "starpu_data_access_mode" 'enumname)) :count STARPU_NMAXBUFS)
         (#.(swig-lispify "cl_arg" 'slotname) "cl_arg" :type :pointer)
         (#.(swig-lispify "cl_arg_size" 'slotname) "cl_arg_size" :type :size)
         (#.(swig-lispify "callback_func" 'slotname) "callback_func" :type :pointer)
         (#.(swig-lispify "callback_arg" 'slotname) "callback_arg" :type :pointer)
         (#.(swig-lispify "status" 'slotname) "status" :type  #.(swig-lispify "starpu_task_status" 'enumname))
         (#.(swig-lispify "type" 'slotname) "type" :type :unsigned-int)
         (#.(swig-lispify "color" 'slotname) "color" :type :unsigned-int)
         (#.(swig-lispify "flops" 'slotname) "flops" :type :double)
         (#.(swig-lispify "predicted" 'slotname) "predicted" :type :double)
         (#.(swig-lispify "predicted_transfer" 'slotname) "predicted_transfer" :type :double)
         (#.(swig-lispify "predicted_start" 'slotname) "predicted_start" :type :double))

(cstruct #.(swig-lispify "starpu_sched_ctx_iterator" 'classname) "struct starpu_sched_ctx_iterator")

(cstruct #.(swig-lispify "starpu_worker_collection" 'classname) "struct starpu_worker_collection")

(cstruct #.(swig-lispify "starpu_perfmodel_device" 'classname) "struct starpu_perfmodel_device")

(cstruct #.(swig-lispify "starpu_perfmodel_arch" 'classname) "struct starpu_perfmodel_arch")

(cstruct #.(swig-lispify "starpu_perfmodel_history_entry" 'classname) "struct starpu_perfmodel_history_entry")

(cstruct #.(swig-lispify "starpu_perfmodel_history_list" 'classname) "struct starpu_perfmodel_history_list")

(cstruct #.(swig-lispify "starpu_perfmodel_regression_model" 'classname) "struct starpu_perfmodel_regression_model")

(cstruct #.(swig-lispify "starpu_perfmodel_per_arch" 'classname) "struct starpu_perfmodel_per_arch")

(cstruct #.(swig-lispify "starpu_perfmodel" 'classname) "struct starpu_perfmodel")

(cstruct #.(swig-lispify "starpu_task_list" 'classname) "struct starpu_task_list")

(cstruct #.(swig-lispify "starpu_codelet_pack_arg_data" 'classname) "struct starpu_codelet_pack_arg_data")

(cstruct #.(swig-lispify "starpu_sched_policy" 'classname) "struct starpu_sched_policy")

(cstruct #.(swig-lispify "starpu_profiling_task_info" 'classname) "struct starpu_profiling_task_info")

(cstruct #.(swig-lispify "starpu_profiling_worker_info" 'classname) "struct starpu_profiling_worker_info")

(cstruct #.(swig-lispify "starpu_profiling_bus_info" 'classname) "struct starpu_profiling_bus_info")

(cstruct #.(swig-lispify "starpu_fxt_codelet_event" 'classname) "struct starpu_fxt_codelet_event")

(cstruct #.(swig-lispify "starpu_fxt_options" 'classname) "struct starpu_fxt_options")

(cstruct #.(swig-lispify "starpu_driver" 'classname) "struct starpu_driver")

(cstruct #.(swig-lispify "starpu_tree" 'classname) "struct starpu_tree")

(cstruct #.(swig-lispify "starpu_conf" 'classname) "struct starpu_conf"
         (#.(swig-lispify "ncpus" 'slotname) "ncpus" :type :int)
         (#.(swig-lispify "reserve_ncpus" 'slotname) "reserve_ncpus" :type :int)
         (#.(swig-lispify "ncuda" 'slotname) "ncuda" :type :int)
         (#.(swig-lispify "nopencl" 'slotname) "nopencl" :type :int)
         (#.(swig-lispify "nmic" 'slotname) "nmic" :type :int)
         (#.(swig-lispify "nmpi_ms" 'slotname) "nmpi_ms" :type :int)
         (#.(swig-lispify "bus_calibrate" 'slotname) "bus_calibrate" :type :int)
         (#.(swig-lispify "calibrate" 'slotname) "calibrate" :type :int)
         (#.(swig-lispify "single_combined_worker" 'slotname) "single_combined_worker" :type :int)
         (#.(swig-lispify "disable_asynchronous_copy" 'slotname) "disable_asynchronous_copy" :type :int)
         (#.(swig-lispify "disable_asynchronous_cuda_copy" 'slotname) "disable_asynchronous_copy" :type :int)
         (#.(swig-lispify "disable_asynchronous_opencl_copy" 'slotname) "disable_asynchronous_copy" :type :int)
         (#.(swig-lispify "disable_asynchronous_mic_copy" 'slotname) "disable_asynchronous_copy" :type :int)
         (#.(swig-lispify "catch_signals" 'slotname) "catch_signals" :type :int))
