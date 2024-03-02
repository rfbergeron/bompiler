# References

A list of places I have gone to online while looking for guidance or solutions
to problems, whether it has to do with project organization, compiler design,
or the meaning of the C standard.

- <https://codereview.stackexchange.com/questions/74136/makefile-that-places-object-files-into-an-alternate-directory-bin>
- <https://stackoverflow.com/questions/19288859/how-to-redirect-stdout-to-a-string-in-ansi-c>
- <https://stackoverflow.com/questions/679979/how-to-make-a-variadic-macro-variable-number-of-arguments>
- <https://blog.microjoe.org/2017/unit-tests-c-cmocka-coverage-cmake.html>
- <https://api.cmocka.org/group__cmocka__alloc.html>
- <https://stackoverflow.com/questions/4317677/c-c-preprocessor-single-quote>
- <https://stackoverflow.com/questions/11442708/type-punning-and-unions-in-c>
- <https://stackoverflow.com/questions/12164855/common-initial-sequence-in-structures-nested-within-union-definition-in-c-stan>
- <https://eklitzke.org/c-functions-without-arguments>
- <http://unixwiz.net/techtips/reading-cdecl.html>
  A note about this one: I figured out how to read types a while ago, but did
  not realize that pointers are read right to left, which only matters when they
  are const or volatile qualified. The source above mentions operator precedence
  when explaining that array and function declarators are read before pointer
  declarators. It does not mention why pointers are read "backwards" in
  comparison, but it is because pointers are right associative.
- <https://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html>
- <https://stackoverflow.com/questions/20024588/simple-x86-64-division-not-working>
  Another note: I didn't realize that for `div` and `idiv` instructions basically
  concatenate the contents of `rax` and `rdx`, so the value you need to put into
  `rdx` changes depending on whether or not the dividend has its top bit set. I
  also didn't realize that `cwd`, `cdq`, and `cqo` existed until reading this.
