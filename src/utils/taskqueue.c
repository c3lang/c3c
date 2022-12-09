#include "lib.h"
#include "compiler_tests/benchmark.h"

// Our task queue is only made for scheduling compilation tasks so there is
// a single thread that adds the tasks.

#if USE_PTHREAD
#include <pthread.h>

typedef struct TaskQueue_
{
	pthread_t *threads;
	volatile int thread_count;
	pthread_mutex_t lock;
	pthread_cond_t notify;
	volatile Task **queue;
	volatile bool shutdown;
	volatile int active_threads;
} TaskQueue;

static void *taskqueue_thread(void *data)
{
	TaskQueue *task_queue = data;
	bool is_active = false;
	while (1)
	{
		pthread_mutex_lock(&task_queue->lock);
		unsigned task_count = vec_size(task_queue->queue);
		if (!task_count || task_queue->shutdown) goto SHUTDOWN;
		if (!is_active)
		{
			task_queue->active_threads++;
			is_active = true;
		}
		Task *task = (Task*)task_queue->queue[task_count - 1];
		vec_pop(task_queue->queue);
		pthread_mutex_unlock(&task_queue->lock);
		task->task(task->arg);
	}
SHUTDOWN:
	if (is_active && --task_queue->active_threads == 0)
	{
		pthread_cond_broadcast(&task_queue->notify);
	}
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
	queue->queue = (volatile Task **)task_list;
	if (pthread_mutex_init(&queue->lock, NULL)) error_exit("Failed to set up mutex");
	if (pthread_cond_init(&queue->notify, NULL)) error_exit("Failed to set up cond");
	pthread_attr_t custom_sched_attr;
	pthread_attr_init(&custom_sched_attr);
	pthread_attr_setscope(&custom_sched_attr, PTHREAD_SCOPE_SYSTEM);
	for (int i = 0; i < threads; i++)
	{
		pthread_t *pthread = &queue->threads[i];

		if (pthread_create(pthread, &custom_sched_attr, taskqueue_thread, queue)) error_exit("Fail to set up thread pool");
		struct sched_param prio = { sched_get_priority_max(SCHED_OTHER) };
		if (pthread_setschedparam(*pthread, SCHED_OTHER, &prio)) error_exit("Failed to set thread priority");
	}
	return queue;
}

void taskqueue_wait_for_completion(TaskQueueRef queue_ref)
{
	TaskQueue *queue = queue_ref;
	assert(queue);
	if (pthread_mutex_lock(&queue->lock) != 0) error_exit("Failed to lock task queue.");
	while (vec_size(queue->queue) || queue->active_threads)
	{
		pthread_cond_wait(&queue->notify, &queue->lock);
	}
	if (pthread_mutex_unlock(&queue->lock) != 0) error_exit("Failed to unlock task queue");
}

void taskqueue_destroy(TaskQueueRef queue_ref)
{
	assert(queue_ref);
	TaskQueue *queue = queue_ref;
	if (pthread_mutex_lock(&queue->lock)) error_exit("Failed to lock task queue.");
	assert(!queue->shutdown);
	vec_resize(queue->queue, 0);
	queue->shutdown = true;
	if (pthread_mutex_unlock(&queue->lock) != 0) error_exit("Failed to unlock task queue.");
	for (int i = 0; i < queue->thread_count; i++)
	{
		if (pthread_join(queue->threads[i], NULL) != 0) error_exit("Failed to join thread.");
	}
	pthread_mutex_destroy(&queue->lock);
	pthread_cond_destroy(&queue->notify);
}

#else

void taskqueue_destroy(TaskQueueRef queue_ref)
{ }

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