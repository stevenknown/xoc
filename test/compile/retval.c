/* Bug c/17855.  */
struct foo {char x;};
struct foo f();
void bar()
{
  f().x = 1;
}
