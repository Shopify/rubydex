void Init_rubydex_rdoc(void) {
    /*
     * Document-module: Rubydex
     *
     * Namespace for Rubydex's Ruby API.
     *
     * This RDoc-only file seeds the C parser with the root module variable
     * before it scans the real extension sources under ext/rubydex.
     */
    mRubydex = rb_define_module("Rubydex");
}
