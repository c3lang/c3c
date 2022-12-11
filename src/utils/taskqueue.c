#include "lib.h"
#include "compiler_tests/benchmark.h"

// Our task queue is only made for scheduling compilation tasks so there is
// a single thread that adds the tasks.

#if USE_PTHREAD
#include <pthread.h>

typedef struct TaskQueue_
{
	pthread_t *threads;
	int thread_count;
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

TaskQueueRef taskqueue_create(int threads, Task **task_list)
{
	assert(threads > 0);
	TaskQueue *queue = CALLOCS(TaskQueue);
	queue->threads = MALLOC(sizeof(pthread_t) * (unsigned)threads);
	queue->thread_count = threads;
	queue->queue = task_list;
	if (pthread_mutex_init(&queue->lock, NULL)) error_exit("Failed to set up mutex");
	for (int i = 0; i < threads; i++)
	{
		if (pthread_create(queue->threads + i, NULL, taskqueue_thread, queue)) error_exit("Fail to set up thread pool");
	}
	return queue;
}

void taskqueue_wait_for_completion(TaskQueueRef queue_ref)
{
	assert(queue_ref);
	TaskQueue *queue = queue_ref;
	for (int i = 0; i < queue->thread_count; i++)
	{
		if (pthread_join(queue->threads[i], NULL) != 0) error_exit("Failed to join thread.");
	}
	pthread_mutex_destroy(&queue->lock);
}


#else

void taskqueue_add(TaskQueueRef queue_ref, Task *task)
{
}

TaskQueueRef taskqueue_create(int threads, Task **tasks)
{
	return tasks;
}

void taskqueue_wait_for_completion(TaskQueueRef queue)
{
	Task **tasks = queue;
	FOREACH_BEGIN(Task *task, tasks)
	task->task(task->arg);
	FOREACH_END();
}

#endif