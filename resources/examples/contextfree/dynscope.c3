module foo;
import std::io;

tlocal String context_user = "safe";

macro long reallyPerform(task)
{
	io::printfn("%s: %s", context_user, task);
	return task.len;
}

macro long perform(task)
{
	return reallyPerform(task);
}

macro @with_mode(String user, #action, ...)
{
	@scope(context_user)
	{
		context_user = user;
		return #action($vasplat);
	};
}

fn void main()
{
	long val1 = perform("something");
	long val2 = @with_mode("faster", perform, "reliable");
	long val3 = perform(String[] {"something"});
	io::printfn("%d %d %d", val1, val2, val3);
}
