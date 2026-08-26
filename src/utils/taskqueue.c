#include "lib.h"
#include "compiler_tests/benchmark.h"

// Our task queue is only made for scheduling compilation tasks so there is
// a single thread that adds the tasks.

#if USE_PTHREAD
#include <pthread.h>

#define TASKQUEUE_THREAD_STACK_SIZE (8U * 1024U * 1024U)

typedef struct TaskQueue_
{
	pthread_mutex_t lock;
	Task **queue;
} TaskQueue;

static void taskqueue_drain(TaskQueue *task_queue)
{
	while (1)
	{
		pthread_mutex_lock(&task_queue->lock);
		int task_count = vec_size(task_queue->queue);
		if (!task_count)
		{
			pthread_mutex_unlock(&task_queue->lock);
			return;
		}
		Task *task = (Task*)task_queue->queue[task_count - 1];
		vec_pop(task_queue->queue);
		pthread_mutex_unlock(&task_queue->lock);
		task->task(task->arg);
	}
}

static void *taskqueue_thread(void *data)
{
	taskqueue_drain((TaskQueue *)data);
	pthread_exit(NULL);
	return NULL;
}

void taskqueue_run(int threads, Task **task_list)
{
	ASSERT(threads > 0);
	if (threads == 1)
	{
		FOREACH(Task *, task, task_list)
		{
			task->task(task->arg);
		}
		return;
	}
	// The calling thread works too, so only threads - 1 extra OS threads are needed.
	int worker_threads = threads - 1;
	pthread_t *pthreads = cmalloc(sizeof(pthread_t) * (int)worker_threads);
	TaskQueue queue = { .queue = task_list };
	pthread_attr_t attr;
	if (pthread_mutex_init(&queue.lock, NULL)) error_exit("Failed to set up mutex");
	if (pthread_attr_init(&attr)) error_exit("Failed to set up attribute for thread");
	size_t stack_size = (size_t)TASKQUEUE_THREAD_STACK_SIZE;
#ifdef PTHREAD_STACK_MIN
	if (stack_size < (size_t)PTHREAD_STACK_MIN) stack_size = (size_t)PTHREAD_STACK_MIN; // NOLINT
#endif
	if (pthread_attr_setstacksize(&attr, stack_size)) error_exit("Failed to set up stack size for thread");
	for (int i = 0; i < worker_threads; i++)
	{
		if (pthread_create(&pthreads[i], &attr, taskqueue_thread, &queue)) error_exit("Fail to set up thread pool");
	}
	pthread_attr_destroy(&attr);
	taskqueue_drain(&queue);
	for (int i = 0; i < worker_threads; i++)
	{
		if (pthread_join(pthreads[i], NULL) != 0) error_exit("Failed to join thread.");
	}
	free(pthreads);
	pthread_mutex_destroy(&queue.lock);
}

#elif PLATFORM_WINDOWS

#include <Windows.h>
#include <process.h>

typedef struct TaskQueue_
{
	CRITICAL_SECTION lock;
	Task **queue;
} TaskQueue;

static void taskqueue_drain(TaskQueue *task_queue)
{
	while (1)
	{
		EnterCriticalSection(&task_queue->lock);
		int task_count = vec_size(task_queue->queue);
		if (!task_count)
		{
			LeaveCriticalSection(&task_queue->lock);
			return;
		}
		Task *task = (Task*)task_queue->queue[task_count - 1];
		vec_pop(task_queue->queue);
		LeaveCriticalSection(&task_queue->lock);
		task->task(task->arg);
	}
}

static unsigned WINAPI taskqueue_thread(LPVOID lpParam)
{
	taskqueue_drain((TaskQueue *)lpParam);
	return 0;
}

void taskqueue_run(int threads, Task **task_list)
{
	ASSERT(threads > 0);
	// The calling thread works too, so only threads - 1 extra OS threads are needed.
	int worker_threads = threads - 1;
	HANDLE *handles = cmalloc(sizeof(HANDLE) * (int)worker_threads);
	TaskQueue queue = { .queue = task_list };
	InitializeCriticalSection(&queue.lock);
	for (int i = 0; i < worker_threads; i++)
	{
		handles[i] = (HANDLE)_beginthreadex(NULL, 0, taskqueue_thread, &queue, 0, NULL);
		if (handles[i] == NULL) error_exit("Fail to set up thread pool");
	}
	taskqueue_drain(&queue);
	if (worker_threads > 0) WaitForMultipleObjects(worker_threads, handles, TRUE, INFINITE);

	for (int i = 0; i < worker_threads; i++)
	{
		CloseHandle(handles[i]);
	}
	free((void*)handles);
	DeleteCriticalSection(&queue.lock);
}

#else

void taskqueue_run(int threads, Task **task_list)
{
	FOREACH(Task *, task, task_list)
	{
		task->task(task->arg);
	}
}

#endif