#!/usr/bin/env python3

# Regenerate the meson.build file with the test cases in the test_suite directory
# Doesn't _need_ to be run, just saves time when adding new test cases
# This consitst of a dict with the test suite name and the stripped path to the test case
# expressions = [
#     '2002-02-13-ConditionalInCall',
#     . . .
#     'casts/cast_expr',
# ]
#
# test_suites = {
#   . . .
#   expressions: expressions,
#   . . .}


import os

def define_meson_tests(root_dir):
    suites = {}
    for dir, subdir, files in os.walk(root_dir):
        # Determine the suite and sub-suite (if any)
        relative_path = os.path.relpath(dir, root_dir)
        path_parts = relative_path.split(os.sep)
        suite = path_parts[0]  # Top-level directory is the suite

        if suite == '.': # we are at the root
            continue

        if len(path_parts) > 1:
            sub_suite = os.path.join(*path_parts[1:]) # Join the rest for the sub-suite path
            test_prefix = f"{sub_suite}/"
        else:
            sub_suite = None
            test_prefix = ""

        test_cases = []
        for file in files:
            if file.endswith(('.c3', '.c3t')):
                test_cases.append(f"{test_prefix}{file}")

        # Add test cases to the suite
        if len(test_cases) > 0:
            if suite not in suites:
                suites[suite] = []
            suites[suite].extend(test_cases)

    # Initialize the output string
    output = []
    output.append('# This file can be regenerated with generate_meson_test_cases.py')
    output.append('')
    output.append('py3 = find_program(\'python3\', required: true)')
    output.append('test_args = [\'../test/src/check.py\', \'../test/src/tester.py\', c3c_exe]')
    output.append('')
    # Build test case lists
    for suite, tests in suites.items():
        output.append(f'{suite} = [')
        for test in tests:
            output.append(f"    '{test}',")
        output.append(']\n')

    # Build test suites dictionary
    output.append('test_suites = {')
    for suite in suites:
        output.append(f"    '{suite}': {suite},")
    output.append('}')

    output.append('''
# Generate tests in meson so that we can make trivial changes manually
foreach suite, tests : test_suites
    foreach test : tests
        test_name = test.replace('.c3', '').replace('.c3t', '')
        test_type = 'compile'
        if test.endswith('.c3t')
            test_type = 'unit'
        endif
        test(
            f'Test Suite - @suite@ - @test_type@ - @test_name@',
            py3,
            args: [test_args, f'../test/test_suite/@suite@/@test@'],
        )
    endforeach
endforeach
'''
    )
    return '\n'.join(output)


# # Example usage with a dummy directory structure (and test cases)
# root_directory = 'test_root'

# # Create a sample directory structure
# os.makedirs(os.path.join(root_directory, 'suite1', 'sub1'), exist_ok=True)
# os.makedirs(os.path.join(root_directory, 'suite1', 'sub2'), exist_ok=True)
# os.makedirs(os.path.join(root_directory, 'suite2'), exist_ok=True)
# os.makedirs(os.path.join(root_directory, 'suite3', 'a', 'b'), exist_ok=True)

# open(os.path.join(root_directory, 'suite1', 'sub1', 'test1.c3t'), 'w').close()
# open(os.path.join(root_directory, 'suite1', 'sub1', 'test2.c3t'), 'w').close()
# open(os.path.join(root_directory, 'suite1', 'sub2', 'test3.c3t'), 'w').close()
# open(os.path.join(root_directory, 'suite1', 'test4.c3t'), 'w').close()
# open(os.path.join(root_directory, 'suite2', 'test5.c3t'), 'w').close()
# open(os.path.join(root_directory, 'suite3', 'a', 'b', 'test6.c3t'), 'w').close()

# # Run the function
# define_meson_tests(root_directory)

# # Clean up the sample directory structure (optional)
# import shutil
# shutil.rmtree(root_directory)

# Usage
meson_tests = define_meson_tests('../test_suite/')

# Write the output to meson.build
with open('../meson.build', 'w') as f:
    f.write(meson_tests)
