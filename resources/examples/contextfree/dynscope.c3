module foo;
import std::io;

tlocal String context_user = "safe";

macro sz reallyPerform(task)
{
	io::printfn("%s: %s", context_user, task);
	return task.len;
}

macro sz perform(task)
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
	sz val1 = perform("something");
	sz val2 = @with_mode("faster", perform, "reliable");
	sz val3 = perform((String[]){"something"});
	io::printfn("%d %d %d", val1, val2, val3);
}
