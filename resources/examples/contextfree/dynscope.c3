module foo;
import std::io;

tlocal char[] context_user = "safe";

macro long perform(task)
{
  io::printf("%s: %s\n", context_user, task);
  return task.len;
}

macro @with_mode(char[] user, #action, arg)
{
  @scope(context_user)
  {
    context_user = user;
    return #action(arg);
  };
}

fn void main()
{
  long result = perform("something!");
  result += @with_mode("faster", perform, "reliable");
  result += perform(char[][] {"again", "more"});
  result += perform(int[<2>] { 56, 99 });
  io::printf("Result: %d\n", result);
}