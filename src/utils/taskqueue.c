#include "lib.h"
#include "compiler_tests/benchmark.h"

// Our task queue is only made for scheduling compilation tasks so there is
// a single thread that adds the tasks.

#if USE_PTHREAD
#include <pthread.h>

typedef struct TaskQueue_
{
	pthread_mutex_t lock;
	Task **queue;
} TaskQueue;

static void *taskqueue_thread(void *data)
{
	TaskQueue *task_queue = data;
	bool is_active = false;
	while (1)
	{
		pthread_mutex_lock(&task_queue->lock);
		unsigned task_count = vec_size(task_queue->queue);
		if (!task_count) goto SHUTDOWN;
		Task *task = (Task*)task_queue->queue[task_count - 1];
		vec_pop(task_queue->queue);
		pthread_mutex_unlock(&task_queue->lock);
		task->task(task->arg);
	}
SHUTDOWN:
	pthread_mutex_unlock(&task_queue->lock);
	pthread_exit(NULL);
	return NULL;
}

void taskqueue_run(int threads, Task **task_list)
{
	assert(threads > 0);
	pthread_t *pthreads = malloc(sizeof(pthread_t) * (unsigned)threads);
	TaskQueue queue = { .queue = task_list };
	if (pthread_mutex_init(&queue.lock, NULL)) error_exit("Failed to set up mutex");
	for (int i = 0; i < threads; i++)
	{
		if (pthread_create(&pthreads[i], NULL, taskqueue_thread, &queue)) error_exit("Fail to set up thread pool");
	}
	for (int i = 0; i < threads; i++)
	{
		if (pthread_join(pthreads[i], NULL) != 0) error_exit("Failed to join thread.");
	}
	free(pthreads);
	pthread_mutex_destroy(&queue.lock);
}

#else


void taskqueue_run(int threads, Task **task_list)
{
	FOREACH_BEGIN(Task *task, task_list)
	task->task(task->arg);
	FOREACH_END();
}

#endif