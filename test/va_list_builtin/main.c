int main(int argc, char **argv) {
  __builtin_va_list args;
  args->fp_offset = 0;
  args->gp_offset = 0;
  args->overflow_arg_area = 0;
  args->reg_save_area = 0;
}
