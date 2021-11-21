#include "lib.h"

// Our task queue is only made for scheduling compilation tasks so there is
// a single thread that adds the tasks.

#if USE_PTHREAD
#include <pthread.h>

typedef struct TaskQueue_
{
	pthread_t *threads;
	int thread_count;
	pthread_mutex_t lock;
	pthread_cond_t notify;
	Task **queue;
	bool shutdown;
	int active_threads;
} TaskQueue;

static void *taskqueue_thread(void *queue)
{
	TaskQueue *task_queue = queue;
	bool was_active = false;
	while (1)
	{
		pthread_mutex_lock(&task_queue->lock);
		if (was_active)
		{
			was_active = false;
			if (--task_queue->active_threads == 0) pthread_cond_broadcast(&task_queue->notify);
		}
		// Wait for a task.
		while (!vec_size(task_queue->queue) && !task_queue->shutdown)
		{
			pthread_cond_wait(&task_queue->notify, &task_queue->lock);
		}
		if (task_queue->shutdown) break;

		Task *task = task_queue->queue[vec_size(task_queue->queue) - 1];
		vec_pop(task_queue->queue);
		task_queue->active_threads++;
		was_active = true;
		pthread_mutex_unlock(&task_queue->lock);
		task->task(task->arg);
	}
	pthread_mutex_unlock(&task_queue->lock);
	pthread_exit(NULL);
	return NULL;
}

TaskQueueRef taskqueue_create(int threads)
{
	assert(threads > 0);
	TaskQueue *queue = CALLOCS(TaskQueue);
	queue->threads = malloc_arena(sizeof(pthread_t) * (unsigned)threads);
	queue->thread_count = threads;
	if (pthread_mutex_init(&queue->lock, NULL)) error_exit("Failed to set up mutex");
	if (pthread_cond_init(&queue->notify, NULL)) error_exit("Failed to set up cond");
	for (int i = 0; i < threads; i++)
	{
		if (pthread_create(&(queue->threads[i]), NULL, taskqueue_thread, queue)) error_exit("Fail to set up thread pool");
	}
	return queue;
}

void taskqueue_add(TaskQueueRef queue_ref, Task *task)
{
	TaskQueue *queue = queue_ref;
	assert(queue);
	if (pthread_mutex_lock(&queue->lock) != 0) error_exit("Failed to lock task queue.");
	vec_add(queue->queue, task);
	if (pthread_cond_signal(&queue->notify) != 0) error_exit("Failed to signal tasks.");
	if (pthread_mutex_unlock(&queue->lock) != 0) error_exit("Failed to unlock task queue");
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
	if (pthread_cond_broadcast(&queue->notify)) error_exit("Failed to signal tasks.");
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
	task->task(task->arg);
}

TaskQueueRef taskqueue_create(int threads)
{
	return NULL;
}

void taskqueue_wait_for_completion(TaskQueueRef queue)
{
}

#endif