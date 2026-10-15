struct foo {
  int a:3;
  int b:8;
  int :0;
  int d:5, e:3, :0;
  int f:17;
  char g;
  int h:32;
};

struct bar {
  short a;
  int b:6;
  int c:13;
  int d;
};
