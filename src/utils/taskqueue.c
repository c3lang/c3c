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

static void *taskqueue_thread(void *data)
{
	TaskQueue *task_queue = data;
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
	ASSERT(threads > 0);
	if (threads == 1)
	{
		FOREACH(Task *, task, task_list)
		{
			task->task(task->arg);
		}
		return;
	}
	pthread_t *pthreads = malloc(sizeof(pthread_t) * (unsigned)threads);
	TaskQueue queue = { .queue = task_list };
	pthread_attr_t attr;
	if (pthread_mutex_init(&queue.lock, NULL)) error_exit("Failed to set up mutex");
	if (pthread_attr_init(&attr)) error_exit("Failed to set up attribute for thread");
	size_t stack_size = TASKQUEUE_THREAD_STACK_SIZE;
#ifdef PTHREAD_STACK_MIN
	if (stack_size < PTHREAD_STACK_MIN) stack_size = PTHREAD_STACK_MIN; // NOLINT
#endif
	if (pthread_attr_setstacksize(&attr, stack_size)) error_exit("Failed to set up stack size for thread");
	for (int i = 0; i < threads; i++)
	{
		if (pthread_create(&pthreads[i], &attr, taskqueue_thread, &queue)) error_exit("Fail to set up thread pool");
	}
	pthread_attr_destroy(&attr);
	for (int i = 0; i < threads; i++)
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

static unsigned WINAPI taskqueue_thread(LPVOID lpParam)
{
	TaskQueue *task_queue = (TaskQueue *)lpParam;
	bool is_active = false;
	while (1)
	{
		EnterCriticalSection(&task_queue->lock);
		unsigned task_count = vec_size(task_queue->queue);
		if (!task_count) goto SHUTDOWN;
		Task *task = (Task*)task_queue->queue[task_count - 1];
		vec_pop(task_queue->queue);
		LeaveCriticalSection(&task_queue->lock);
		task->task(task->arg);
	}
SHUTDOWN:
	LeaveCriticalSection(&task_queue->lock);
	return 0;
}

void taskqueue_run(int threads, Task **task_list)
{
	ASSERT(threads > 0);
	HANDLE *handles = malloc(sizeof(HANDLE) * (unsigned)threads);
	TaskQueue queue = { .queue = task_list };
	InitializeCriticalSection(&queue.lock);
	for (int i = 0; i < threads; i++)
	{
		handles[i] = (HANDLE)_beginthreadex(NULL, 0, taskqueue_thread, &queue, 0, NULL);
		if (handles[i] == NULL) error_exit("Fail to set up thread pool");
	}
	WaitForMultipleObjects(threads, handles, TRUE, INFINITE);

	for (int i = 0; i < threads; i++)
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