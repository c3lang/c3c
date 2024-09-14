# we take in a build, and the format to package it with,
# and then package it with the specified format (deb or RPM)
build_path=$1
format=$2
EXIT_CODE_FMT_NOT_IMPLEMENTED=2
EXIT_CODE_FMT_UNKNOWN=3
EXIT_CODE_FAIL=1

fmt_not_impl() {
    echo "NOT IMPLEMENTED: format '$format' not implemented yet"
    exit $EXIT_CODE_FMT_NOT_IMPLEMENTED
}

if [ "$format" = "deb" ]; then
    fmt_not_impl
elif [ "$format" = "rpm" ]; then
    fmt_not_impl
else
    echo "unknown format: $format"
    exit $EXIT_CODE_FMT_UNKNOWN
fi