%module starpu

%feature("intern_function", "1");

%inline %{typedef signed char int8_t; %}
%inline %{typedef short int int16_t; %}
%inline %{typedef int int32_t; %}
%inline %{typedef long int int64_t; %}
%inline %{typedef unsigned char uint8_t; %}
%inline %{typedef unsigned short int uint16_t; %}
%inline %{typedef unsigned int uint32_t; %}
%inline %{typedef unsigned long int uint64_t; %}
%inline %{typedef long unsigned int size_t; %}
%inline %{typedef long int ssize_t; %}

%include "starpu/1.3/starpu_config.h"
%include "starpu/1.3/starpu_util.h"
%include "cl-starpu-tweaks.h"
%include "starpu/1.3/starpu_thread.h"
%include "starpu/1.3/starpu_thread_util.h"
%include "starpu/1.3/starpu_data.h"
%include "starpu/1.3/starpu_helper.h"
%include "starpu/1.3/starpu_disk.h"
%include "starpu/1.3/starpu_data_interfaces.h"
%include "starpu/1.3/starpu_data_filters.h"
%include "starpu/1.3/starpu_stdlib.h"
%include "starpu/1.3/starpu_task_bundle.h"
%include "starpu/1.3/starpu_task_dep.h"
%include "starpu/1.3/starpu_task.h"
%include "starpu/1.3/starpu_worker.h"
%include "starpu/1.3/starpu_perfmodel.h"
%include "starpu/1.3/starpu_worker.h"
%include "starpu/1.3/starpu_task_list.h"
%include "starpu/1.3/starpu_task_util.h"
%include "starpu/1.3/starpu_scheduler.h"
%include "starpu/1.3/starpu_sched_ctx.h"
%include "starpu/1.3/starpu_expert.h"
%include "starpu/1.3/starpu_rand.h"
%include "starpu/1.3/starpu_cuda.h"
%include "starpu/1.3/starpu_cublas.h"
%include "starpu/1.3/starpu_cusparse.h"
%include "starpu/1.3/starpu_bound.h"
%include "starpu/1.3/starpu_hash.h"
%include "starpu/1.3/starpu_profiling.h"
%include "starpu/1.3/starpu_fxt.h"
%include "starpu/1.3/starpu_driver.h"
%include "starpu/1.3/starpu_tree.h"
%include "starpu/1.3/starpu_simgrid_wrap.h"
%include "starpu/1.3/starpu_bitmap.h"
%include "starpu/1.3/starpu_clusters.h"
%include "starpu/1.3/starpu.h"
