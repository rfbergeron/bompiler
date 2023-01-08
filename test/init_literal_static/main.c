const char deduced[] = "Hello, World!";
const char explicit[18] = "Greetings, Globe!";
const char not_enough[40] = "Salutations, Sphere!";
const char too_many[6] = "hello world";
const char just_right[12] = "Ahoy, Atlas!";

int main(int argc, char **argv) {
  static const char local[] = "foo bar baz";
  return 0;
}
